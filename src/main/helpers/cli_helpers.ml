module LigoRC = Ligo_rc
module LigoIgnore = Ligo_ignore
module Constants = Constants
module Trace = Simple_utils.Trace
module Display = Simple_utils.Display

type unzip_error = UnableToUnzip

let unzip fname =
  let in_fd = Ligo_unix.openfile fname [ Ligo_unix.O_RDONLY ] 0 in
  let file_size = (Ligo_unix.stat fname).st_size in
  let buffer_len = De.io_buffer_size in
  let i = De.bigstring_create De.io_buffer_size in
  let o = De.bigstring_create De.io_buffer_size in
  let r = Buffer.create 0x1000 in
  let p = ref 0 in
  let refill buf =
    let len = min (file_size - !p) buffer_len in
    if len <= 0
    then 0
    else (
      let bytes = Bytes.create len in
      let len = Ligo_unix.read in_fd bytes 0 len in
      Bigstringaf.blit_from_bytes bytes ~src_off:0 buf ~dst_off:0 ~len;
      p := !p + len;
      len)
  in
  let flush buf len =
    let str = Bigstringaf.substring buf ~off:0 ~len in
    Buffer.add_string r str
  in
  match Gz.Higher.uncompress ~refill ~flush i o with
  | Ok _ ->
    let bytes = Buffer.contents_bytes r in
    let nbytes = Bytes.length bytes in
    let fname = Format.sprintf "%s.tar" (Caml.Filename.remove_extension fname) in
    let out_fd = Ligo_unix.openfile fname [ Ligo_unix.O_CREAT; Ligo_unix.O_RDWR ] 0o666 in
    let mbytes = Ligo_unix.write out_fd bytes 0 nbytes in
    let () = Ligo_unix.close in_fd in
    let () = Ligo_unix.close out_fd in
    if nbytes = mbytes then Ok fname else Error UnableToUnzip
  | Error (`Msg _) ->
    let () = Ligo_unix.close in_fd in
    Error UnableToUnzip


let touch f = Ligo_unix.openfile f [ Ligo_unix.O_CREAT ] 0o666 |> Ligo_unix.close

let untar ~dest_dir fname =
  let fd = Ligo_unix.openfile fname [ Ligo_unix.O_RDONLY ] 0 in
  let move f =
    let f = Filename.concat dest_dir f in
    let () = Ligo_unix.mkdir_p ~perm:0o755 (Filename.dirname f) in
    let () = touch f in
    f
  in
  let () = Tar_unix.Archive.extract move fd in
  Ligo_unix.close fd


module Checksum : sig
  type error = IntegrityMismatch

  val string_of_error : error -> string
  val sha1_bytes : bytes -> string
  val sha1 : string -> string
  val check_integrity : string -> expected:string -> (unit, error) result
end = struct
  type error = IntegrityMismatch

  let string_of_error = function
    | IntegrityMismatch -> "Error : integrity checksum failed"


  let sha1_bytes s = s |> Digestif.SHA1.digest_bytes |> Digestif.SHA1.to_hex
  let sha1 s = s |> Digestif.SHA1.digest_string |> Digestif.SHA1.to_hex

  let check_integrity fname ~expected =
    let fd = Ligo_unix.openfile fname [ Ligo_unix.O_RDONLY ] 0 in
    let file_size = (Ligo_unix.stat fname).st_size in
    let buf = Bytes.create file_size in
    let rec read idx =
      let len = if idx + 512 > file_size then file_size - idx else 512 in
      let _ = Ligo_unix.read fd buf idx len in
      if len < 512 then () else read (idx + len)
    in
    let () = read 0 in
    let () = Ligo_unix.close fd in
    let got = sha1_bytes buf in
    if String.equal got expected then Ok () else Error IntegrityMismatch
end

let find_project_root () =
  let pwd = Caml.Sys.getcwd in
  let rec aux p =
    let dirs = Ligo_unix.ls_dir p in
    if List.exists ~f:(String.equal "ligo.json") dirs
    then Some p
    else (
      let p' = Filename.dirname p in
      (* Check if we reached the root directory, since the parent of 
         the root directory is the root directory itself *)
      if Filename.equal p p' then None else aux p')
  in
  try
    aux (pwd ()) (* In case of permission issues when reading file, catch the exception *)
  with
  | _ -> None


let return_good ?output_file v =
  let fmt : Format.formatter =
    match output_file with
    | Some file_path -> Format.formatter_of_out_channel @@ Out_channel.create file_path
    | None -> Format.std_formatter
  in
  Format.fprintf fmt "%s\n" v;
  Format.pp_print_flush fmt ()


let return_bad v : unit =
  if Char.(v.[String.length v - 1] = '\n')
  then Format.eprintf "%s" v
  else Format.eprintf "%s\n" v;
  Format.pp_print_flush Format.err_formatter ()


let return_with_warn ~show_warnings warns f =
  if (not (String.length (String.strip warns) = 0)) && show_warnings
  then (
    Format.eprintf "%s\n" warns;
    Format.pp_print_flush Format.err_formatter ());
  f ()


type return =
  | Done
  | Compileur_Error
  | Exception of exn

let return_with_custom_formatter ~cli_analytics ~skip_analytics
    :  return:return ref -> ?show_warnings:bool -> ?output_file:string
    -> (unit -> ('value, _) result) -> unit
  =
 fun ~return ?(show_warnings = false) ?output_file f ->
  Analytics.propose_term_acceptation ~skip_analytics;
  let _ =
    try
      match f () with
      | Ok (v, w) ->
        return := Done;
        return_with_warn ~show_warnings w (fun () -> return_good ?output_file v)
      | Error (e, w) ->
        return := Compileur_Error;
        return_with_warn ~show_warnings w (fun () -> return_bad e)
    with
    | exn -> return := Exception exn
  in
  Analytics.edit_metrics_values cli_analytics;
  match !return with
  | Done -> Analytics.push_collected_metrics ~skip_analytics
  | Compileur_Error -> ()
  | Exception e ->
    let _e = Format.asprintf "exception %a" Exn.pp e in
    ()


let return_result ~cli_analytics ~skip_analytics
    :  return:return ref -> ?show_warnings:bool -> ?output_file:string -> display_format:_
    -> no_colour:bool -> warning_as_error:bool
    -> 'value Display.format
       * (raise:(Main_errors.all, Main_warnings.all) Trace.raise
          -> 'value * Analytics.analytics_inputs)
    -> unit
  =
 fun ~return
     ?(show_warnings = false)
     ?output_file
     ~display_format
     ~no_colour
     ~warning_as_error
     (value_format, f) ->
  Analytics.propose_term_acceptation ~skip_analytics;
  let () =
    try
      let result = Trace.to_stdlib_result f in
      let value, analytics =
        match result with
        | Ok ((v, analytics), _w) -> Ok v, analytics
        | Error (e, _w) -> Error e, []
      in
      let format = Display.bind_format value_format Main_errors.Formatter.error_format in
      let formatted_result () =
        Ligo_api.Api_helpers.toplevel
          ~warning_as_error
          ~display_format
          ~no_colour
          (Displayable { value; format })
          result
      in
      Analytics.edit_metrics_values (List.append cli_analytics analytics);
      match formatted_result () with
      | Ok (v, w) ->
        return := Done;
        return_with_warn ~show_warnings w (fun () -> return_good ?output_file v)
      | Error (e, w) ->
        return := Compileur_Error;
        return_with_warn ~show_warnings w (fun () -> return_bad e)
    with
    | exn -> return := Exception exn
  in
  (* Push analytics *)
  match !return with
  | Done -> Analytics.push_collected_metrics ~skip_analytics
  | Compileur_Error -> ()
  | Exception _ -> ()


type command = string * string array

(* Checks if executable is present *)
let does_command_exist (cmd : string) =
  let cmd =
    if String.equal Sys.os_type Constants.windows
    then Constants.where ~cmd
    else Constants.which ~cmd
  in
  let exit = Lwt_process.exec cmd in
  let status = Lwt_main.run exit in
  match status with
  | WEXITED 0 -> Ok true
  | WEXITED 1 -> Ok false
  | _ -> Error "unknown error"


let makeCommand cmd =
  let open Caml in
  (* Looks up $env:PATH for the command *)
  let resolve_npm_command cmd =
    let pathVars =
      Ligo_unix.environment ()
      |> Array.to_list
      |> List.filter_map (fun e ->
             match Str.split (Str.regexp "=") e with
             | k :: v :: _rest -> Some (k, v)
             | _ -> None)
      |> List.filter (fun e ->
             match e with
             | k, _ -> String.lowercase_ascii k = "path")
    in
    let path_sep =
      match Sys.unix with
      | true -> ":"
      | false -> ";"
    in
    let path_sep_regexp = Str.regexp path_sep in
    let v =
      List.fold_right
        (fun e acc ->
          let _, v = e in
          acc ^ path_sep ^ v)
        pathVars
        ""
    in
    let paths = Str.split path_sep_regexp v in
    let npmPaths =
      List.filter_map
        (fun path ->
          let cmd_p = Filename.concat path (sprintf "%s.cmd" cmd) in
          let exe_p = Filename.concat path (sprintf "%s.exe" cmd) in
          if Sys.file_exists cmd_p
          then Some cmd_p
          else if Sys.file_exists exe_p
          then Some exe_p
          else None)
        paths
    in
    match npmPaths with
    | [] ->
      fprintf stderr "No command %s found in environment" cmd;
      exit (-1)
    | h :: _ -> h
  in
  match Sys.unix with
  | true -> cmd
  | false -> resolve_npm_command cmd


let run ?env c args =
  let open Ligo_unix in
  let env_vars =
    match env with
    | Some v -> v
    | None -> Ligo_unix.environment ()
  in
  let pid =
    Ligo_unix.create_process_env
      c
      (Array.append [| c |] args)
      env_vars
      stdin
      stdout
      stderr
  in
  match Ligo_unix.waitpid [] pid with
  | _, WEXITED c -> c
  | _, WSIGNALED c -> c
  | _, WSTOPPED c -> c


let run ?env cmd args =
  let exitCode = run ?env cmd args in
  if exitCode <> 0
  then
    printf
      "%s failed. Exit code relayed to the shell\n Exiting with (%d)...\n"
      cmd
      exitCode


(* Runs a commands in a separate process *)
let run_command (cmd : command) =
  if Sys.unix
  then (
    let status =
      Lwt_process.with_process_none ~stdout:`Keep ~stderr:`Keep cmd (fun p ->
          Lwt.map
            (fun status ->
              match status with
              | Ligo_unix.WEXITED 0 -> Ok ()
              | _ -> Error "unknown error")
            p#status)
    in
    Lwt_main.run status)
  else (
    let _empty_string, args = cmd in
    let bin = args.(0) in
    let bin_full_path = makeCommand bin in
    let env =
      Ligo_unix.environment ()
      |> Array.filter ~f:(fun kv -> not (Str.string_match (Str.regexp "_=") kv 0))
      |> Array.append [| "_=" |]
    in
    let args = Array.sub args ~pos:1 ~len:(Array.length args - 1) in
    run ~env bin_full_path args;
    Ok ())

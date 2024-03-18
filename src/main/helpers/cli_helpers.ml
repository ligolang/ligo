module LigoRC = Ligo_rc
module LigoIgnore = Ligo_ignore
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


let return_result_lwt ?(fast_fail = true) ~cli_analytics ~skip_analytics
    :  return:return ref -> ?show_warnings:bool -> ?output_file:string
    -> ?minify_json:bool -> display_format:_ -> no_colour:bool -> warning_as_error:bool
    -> 'value Display.format
       * (raise:(Main_errors.all, Main_warnings.all) Trace.raise
          -> ('value * Analytics.analytics_inputs) Lwt.t)
    -> unit
  =
 fun ~return
     ?(show_warnings = false)
     ?output_file
     ?(minify_json = false)
     ~display_format
     ~no_colour
     ~warning_as_error
     (value_format, f) ->
  Analytics.propose_term_acceptation ~skip_analytics;
  let get_formatted_result () =
    let edit_metrics_and_format_toplevel result format =
      let value, analytics =
        match result with
        | Ok ((v, analytics), _e, _w) -> Ok v, analytics
        | Error (e, _w) -> Error e, []
      in
      Analytics.edit_metrics_values (List.append cli_analytics analytics);
      Ligo_api.Api_helpers.toplevel
        ~warning_as_error
        ~minify_json
        ~display_format
        ~no_colour
        (Displayable { value; format })
        result
    in
    if fast_fail
    then (
      let result = Lwt_main.run @@ Trace.to_stdlib_result_lwt ~fast_fail:Fast_fail f in
      let format = Display.bind_format value_format Main_errors.Formatter.error_format in
      edit_metrics_and_format_toplevel result format)
    else (
      let result = Lwt_main.run @@ Trace.to_stdlib_result_lwt ~fast_fail:No_fast_fail f in
      let format = Display.bind_format value_format Main_errors.Formatter.errors_format in
      edit_metrics_and_format_toplevel result format)
  in
  let () =
    try
      match get_formatted_result () with
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


let return_result ?fast_fail ~cli_analytics ~skip_analytics
    :  return:return ref -> ?show_warnings:bool -> ?output_file:string
    -> ?minify_json:bool -> display_format:_ -> no_colour:bool -> warning_as_error:bool
    -> 'value Display.format
       * (raise:(Main_errors.all, Main_warnings.all) Trace.raise
          -> 'value * Analytics.analytics_inputs)
    -> unit
  =
 fun ~return
     ?show_warnings
     ?output_file
     ?minify_json
     ~display_format
     ~no_colour
     ~warning_as_error
     (value_format, f) ->
  return_result_lwt
    ?fast_fail
    ~cli_analytics
    ~skip_analytics
    ~return
    ?show_warnings
    ?output_file
    ?minify_json
    ~display_format
    ~no_colour
    ~warning_as_error
    (value_format, fun ~raise -> Lwt.return @@ f ~raise)

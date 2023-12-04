module Constants = Constants

type command = string * string array

(* Checks if executable is present *)
let does_command_exist (cmd : string) =
  let cmd =
    if String.equal Sys.os_type Constants.windows
    then Constants.where ~cmd
    else Constants.which ~cmd
  in
  let exit = Lwt_process.exec ~stdout:`Dev_null cmd in
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
let run_command ?cwd ?(stdout = `Keep) ?(stderr = `Keep) (cmd : command) =
  if Sys.unix
  then (
    let status =
      Lwt_process.with_process_none ?cwd ~stdout ~stderr cmd (fun p ->
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

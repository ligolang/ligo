module LigoRC = Ligo_rc
module LigoManifest = Ligo_manifest
module LigoIgnore = Ligo_ignore
module RepositoryUrl = Repository_url
module Constants = Constants
module Semver = LigoManifest.Semver

let find_project_root () =
  let pwd = Caml.Sys.getcwd in
  let rec aux p =
    let dirs = Ligo_unix.ls_dir p in
    if List.exists ~f:(String.equal "package.json") dirs
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

let return_result
    :  return:return ref -> ?show_warnings:bool -> ?output_file:string
    -> (unit -> ('value, _) result) -> unit
  =
 fun ~return ?(show_warnings = false) ?output_file f ->
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


(* Runs a commands in a separate process *)
let run_command (cmd : command) =
  let status =
    Lwt_process.with_process_none ~stdout:`Keep ~stderr:`Keep cmd (fun p ->
        Lwt.map
          (fun status ->
            match status with
            | Ligo_unix.WEXITED 0 -> Ok ()
            | _ -> Error "unknown error")
          p#status)
  in
  Lwt_main.run status

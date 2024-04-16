module Constants = Constants

type command = string * string array

(* Checks if executable is present *)
let does_command_exist (cmd : string) =
  let cmd = Constants.which ~cmd in
  let exit = Lwt_process.exec ~stdout:`Dev_null cmd in
  let status = Lwt_main.run exit in
  match status with
  | WEXITED 0 -> Ok true
  | WEXITED 1 -> Ok false
  | _ -> Error "unknown error"


(* Runs a commands in a separate process *)
let run_command ?cwd ?(stdout = `Keep) ?(stderr = `Keep) (cmd : command) =
  let status =
    Lwt_process.with_process_none ?cwd ~stdout ~stderr cmd (fun p ->
        Lwt.map
          (fun status ->
            match status with
            | Ligo_unix.WEXITED 0 -> Ok ()
            | _ -> Error "unknown error")
          p#status)
  in
  Lwt_main.run status

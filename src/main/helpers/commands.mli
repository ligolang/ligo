module Constants = Constants

type command = string * string array

(* Checks if executable is present *)
val does_command_exist : string -> (bool, string) result

(* Runs a commands in a separate process *)
val run_command
  :  ?cwd:string
  -> ?stdout:Lwt_process.redirection
  -> ?stderr:Lwt_process.redirection
  -> command
  -> (unit, string) result

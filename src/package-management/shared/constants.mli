(** Contains constants relevant to package management *)

type command = string * string array

(** Returns the "ligo compile contract ..." command, parameterised by main entry point, ligo binary path and expression *)
val ligo_compile_storage
  :  ?ligo:string
  -> main:string
  -> expression:string
  -> unit
  -> command

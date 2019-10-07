(*
type file_location = {
  filename : string ;
  start_line : int ;
  start_column : int ;
  end_line : int ;
  end_column : int ;
}

type virtual_location = string

type t =
  | File of file_location
  | Virtual of virtual_location

val make : Lexing.position -> Lexing.position -> t

val virtual_location : string -> t
val dummy : string
*)

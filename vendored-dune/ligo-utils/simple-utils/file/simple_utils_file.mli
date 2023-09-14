(* Source file ops. Could be on a file. Or served over HTTP *)
val exists : ?dir:string -> string -> string option
val read : string -> string option

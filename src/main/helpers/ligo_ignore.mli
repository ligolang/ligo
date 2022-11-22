type t = Re.re list

val read : ligoignore_path:string -> t
val matches : t -> string -> bool
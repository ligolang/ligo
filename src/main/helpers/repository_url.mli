type t

val parse : Yojson.Safe.t -> t
val to_yojson : t -> Yojson.Safe.t
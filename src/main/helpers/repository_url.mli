type t

val parse : Yojson.Safe.t -> (t, string) result
val to_yojson : t -> Yojson.Safe.t

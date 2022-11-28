type t = private
  { type_ : string
  ; url : string
  ; directory : string option
  }

val parse : Yojson.Safe.t -> (t, string) result
val to_yojson : t -> Yojson.Safe.t

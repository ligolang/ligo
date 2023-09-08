type t = private
  { type_ : string
  ; url : string
  ; directory : string option
  }
[@@deriving yojson]

val parse : Yojson.Safe.t -> (t, string) result

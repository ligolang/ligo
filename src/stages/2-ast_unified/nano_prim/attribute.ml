type t =
  { key : string
  ; value : string option [@sexp.option]
  }
[@@deriving yojson, iter, fold, sexp, eq, compare, hash]

let make key v = { key; value = Some v }

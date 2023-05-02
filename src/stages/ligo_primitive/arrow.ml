type 'a t =
  { type1 : 'a
  ; type2 : 'a
  }
[@@deriving eq, compare, yojson, hash, fold, map, sexp]

let pp g ppf { type1; type2 } = Format.fprintf ppf "%a -> %a" g type1 g type2

type 'e t =
  { condition : 'e
  ; then_clause : 'e
  ; else_clause : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f ppf { condition; then_clause; else_clause } =
  Format.fprintf ppf "if %a then %a else %a" f condition f then_clause f else_clause

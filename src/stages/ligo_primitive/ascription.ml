type ('e, 't) t =
  { anno_expr : 'e
  ; type_annotation : 't
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f g ppf { anno_expr; type_annotation } =
  Format.fprintf ppf "%a : %a" f anno_expr g type_annotation

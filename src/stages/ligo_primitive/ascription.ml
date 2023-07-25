type ('e, 't) t =
  { anno_expr : 'e
  ; type_annotation : 't
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f g ppf { anno_expr; type_annotation } =
  Format.fprintf ppf "%a : %a" f anno_expr g type_annotation


let fold_map
    :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a, 'c) t
    -> 'acc * ('b, 'd) t
  =
 fun f g acc { anno_expr; type_annotation } ->
  let acc, anno_expr = f acc anno_expr in
  let acc, type_annotation = g acc type_annotation in
  acc, { anno_expr; type_annotation }

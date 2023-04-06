type ('expr, 'block) case =
  | Switch_case of 'expr * 'block option
  | Switch_default_case of 'block option

and ('expr, 'block) t =
  { switchee : 'expr
  ; cases : ('expr, 'block) case Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

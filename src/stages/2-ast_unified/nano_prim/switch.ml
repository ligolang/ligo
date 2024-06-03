module Ne_list = Simple_utils.Ne_list

type ('expr, 'block) t =
  { subject : 'expr
  ; cases : ('expr, 'block) switch_cases
  }

and ('expr, 'block) switch_cases =
  | AllCases of
      ('expr, 'block) switch_case Ne_list.t * ('block option option[@sexp.option])
  | Default of 'block option

and ('expr, 'block) switch_case =
  { expr : 'expr
  ; case_body : 'block option [@sexp.option]
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

type ('pattern, 'branch) clause =
  { pattern : 'pattern option
  ; rhs : 'branch
  }

and ('expr, 'pattern, 'branch) t =
  { expr : 'expr
  ; disc_label : Ligo_prim.Label.t option
  ; cases : ('pattern, 'branch) clause Simple_utils.List.Ne.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

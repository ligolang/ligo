module Ne_list = Simple_utils.Ne_list
module Label = Ligo_prim.Label

type ('pattern, 'branch) clause =
  { pattern : 'pattern option
  ; rhs : 'branch
  }

and ('expr, 'pattern, 'branch) t =
  { expr : 'expr
  ; disc_label : Label.t option
  ; cases : ('pattern, 'branch) clause Ne_list.t
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

[@@@warning "-30"]

type ('pattern, 'expr, 'block) t =
  | ForMap of ('expr, 'block) for_map
  | ForSetOrList of ('expr, 'block) for_set_or_list
  | ForAny of ('pattern, 'expr, 'block) for_any

and ('pattern, 'expr, 'block) for_any =
  { pattern : 'pattern
  ; collection : 'expr
  ; block : 'block
  }

and ('expr, 'block) for_map =
  { binding : Ligo_prim.Value_var.t * Ligo_prim.Value_var.t
  ; collection : 'expr
  ; block : 'block
  }

and ('expr, 'block) for_set_or_list =
  { var : Ligo_prim.Value_var.t
  ; for_kind : [ `Set | `List ]
  ; collection : 'expr
  ; block : 'block
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

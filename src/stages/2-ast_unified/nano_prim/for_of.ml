type ('expr, 'statement) t =
  { index_kind : [ `Let | `Const ]
  ; index : Ligo_prim.Value_var.t
  ; expr : 'expr
  ; for_stmt : 'statement
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

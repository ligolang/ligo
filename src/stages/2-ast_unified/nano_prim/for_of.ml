type ('expr, 'pattern, 'statement) t =
  { index_kind : [ `Let | `Const ]
  ; index : 'pattern
  ; expr : 'expr
  ; for_stmt : 'statement
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

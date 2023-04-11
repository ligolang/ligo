type ('expr, 'block) t =
  { index : Ligo_prim.Value_var.t
  ; init : 'expr
  ; bound : 'expr
  ; step : 'expr option (* [1] if [None] *)
  ; block : 'block
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

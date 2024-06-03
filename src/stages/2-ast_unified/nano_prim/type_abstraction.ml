module Type_var = Ligo_prim.Type_var

type 'expr t =
  { type_binder : Type_var.t
  ; result : 'expr
  }
[@@deriving eq, compare, yojson, sexp, fold, iter, map, compare, hash]

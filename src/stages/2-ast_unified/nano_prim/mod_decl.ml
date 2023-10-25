type 'signature annotation =
  { signatures : 'signature list
  ; filter : bool
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

type ('mod_expr, 'signature) t =
  { name : Ligo_prim.Module_var.t
  ; mod_expr : 'mod_expr
  ; annotation : 'signature annotation
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

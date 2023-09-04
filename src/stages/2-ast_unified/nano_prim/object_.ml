type 'expr field_id =
  | F_Name of Ligo_prim.Label.t
  | F_Int of Simple_utils.Z.t
  | F_Str of string

and 'expr property =
  { field_id : 'expr field_id
  ; field_rhs : 'expr option
  }

and 'expr t = 'expr property list

and 'expr update =
  { object_ : 'expr
  ; updates : 'expr t
  }
[@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]

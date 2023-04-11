type field_lens =
  | Lens_Id
  | Lens_Add
  | Lens_Sub
  | Lens_Mult
  | Lens_Div
  | Lens_Fun

and 'expr full_field =
  { field_lhs : 'expr Selection.t list
  ; field_lens : field_lens
  ; field_rhs : 'expr
  }

and 'expr field =
  | Pun of Ligo_prim.Label.t Simple_utils.Location.wrap
  | Full_field of 'expr full_field

and 'expr t =
  { structure : 'expr
  ; update : 'expr field list
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

[@@@warning "-30"]

type assignment_operator =
  | Times_eq
  | Div_eq
  | Min_eq
  | Plus_eq
  | Mod_eq
  | BitAnd_eq
  | BitOr_eq
  | BitXor_eq
  | BitSl_eq
  | BitSr_eq

and operator =
  | Eq
  | Assignment_operator of assignment_operator

and 'expr structural =
  { expr1 : 'expr
  ; op : operator
  ; expr2 : 'expr
  }

and 'expr assign =
  { var : Ligo_prim.Value_var.t
  ; op : operator
  ; rhs : 'expr
  ; returned : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

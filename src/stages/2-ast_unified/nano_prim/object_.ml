module Location = Simple_utils.Location
module Label = Ligo_prim.Label
module Ligo_z = Simple_utils.Ligo_z

type 'expr field_id =
  | F_Name of Label.t
  | F_Int of Ligo_z.t
  | F_Str of string

and 'expr property =
  { field_id : 'expr field_id
  ; field_rhs : 'expr option
  }

and 'expr t = 'expr property Location.wrap list

and 'expr update =
  { object_ : 'expr
  ; updates : 'expr t
  }
[@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]

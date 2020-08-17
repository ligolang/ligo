type expression_
and expression_variable = expression_ Var.t Location.wrap
type type_
and type_variable = type_ Var.t

type label = Label of string
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)
type 'a label_map = 'a LMap.t

and ('a,'b) binder = ('a * 'b)
include Enums
include Enums_utils

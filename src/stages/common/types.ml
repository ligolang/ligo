type expression_
and expression_variable = expression_ Var.t Location.wrap
type type_
and type_variable = type_ Var.t

type constructor' = Constructor of string
type label = Label of string

module CMap = Map.Make( struct type t = constructor' let compare (Constructor a) (Constructor b) = compare a b end)
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)


type 'a label_map = 'a LMap.t
type 'a constructor_map = 'a CMap.t

include Enums
include Enums_utils

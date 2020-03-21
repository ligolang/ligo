module S = Ast_core

(* include Stage_common.Types *)
(* type expression_
 * and expression_variable = expression_ Var.t
 * type type_
 * and type_variable = type_ Var.t *)
type expression_ = Stage_common.Types.expression_
type expression_variable = Stage_common.Types.expression_variable
type type_ = Stage_common.Types.type_
type type_variable = Stage_common.Types.type_variable

type constructor' =
| Constructor of string
type label =
| Label of string

module CMap = Map.Make( struct type t = constructor' let compare (Constructor a) (Constructor b) = compare a b end)
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)

type 'a label_map = 'a LMap.t
type 'a constructor_map = 'a CMap.t
type type_meta = S.type_expression option

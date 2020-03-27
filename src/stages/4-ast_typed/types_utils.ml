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

type 'a location_wrap = 'a Location.wrap
type 'a list_ne = 'a List.Ne.t
type packed_internal_operation = Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation
type location = Location.t
type inline = bool

let fold_map__constructor_map : 'a . 'a constructor_map -> 'state -> ('a -> 'state -> 'new_a * 'state) -> 'new_a constructor_map * 'state = fun _ _ _ -> failwith "TODO fold_map__constructor_map"

let fold_map__label_map : 'a . 'a label_map -> 'state -> ('a -> 'state -> 'new_a * 'state) -> 'new_a label_map * 'state = fun _ _ _ -> failwith "TODO fold_map__label_map"

let fold_map__list : 'a . 'a list -> 'state -> ('a -> 'state -> 'new_a * 'state) -> 'new_a list * 'state = fun l state f ->
  let aux (state, l) element = let (new_element, state) = f element state in (state, new_element::l) in
  let (state, l) = List.fold_left aux (state, []) l in
  (l, state)

let fold_map__location_wrap : 'a . 'a location_wrap -> 'state -> ('a -> 'state -> 'new_a * 'state) -> 'new_a location_wrap * 'state = fun _ _ _ -> failwith "TODO fold_map__location_wrap"

let fold_map__list_ne : 'a . 'a list_ne -> 'state -> ('a -> 'state -> 'new_a * 'state) -> 'new_a list_ne * 'state = fun _ _ _ -> failwith "TODO fold_map__location_wrap"

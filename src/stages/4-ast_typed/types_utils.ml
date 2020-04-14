module S = Ast_core
open Simple_utils.Trace

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

let fold_map__constructor_map : type a new_a state . (state -> a -> (state * new_a) result) -> state -> a constructor_map -> (state * new_a constructor_map) result =
  fun f state m ->
  let aux k v acc =
    let%bind (state , m) = acc in
    let%bind (state , new_v) = f state v in
    ok (state , CMap.add k new_v m) in
  let%bind (state , m) = CMap.fold aux m (ok (state, CMap.empty)) in
  ok (state , m)

let fold_map__label_map : type a state new_a . (state -> a -> (state * new_a) result) -> state -> a label_map -> (state * new_a label_map) result =
  fun f state m ->
  let aux k v acc =
    let%bind (state , m) = acc in
    let%bind (state , new_v) = f state v in
    ok (state , LMap.add k new_v m) in
  let%bind (state , m) = LMap.fold aux m (ok (state, LMap.empty)) in
  ok (state , m)

let fold_map__list : type a state new_a . (state -> a -> (state * new_a) result) -> state -> a list -> (state * new_a list) Simple_utils.Trace.result =
  fun f state l ->
  let aux acc element =
    let%bind state , l = acc in
    let%bind (state , new_element) = f state element in ok (state , new_element :: l) in
  let%bind (state , l) = List.fold_left aux (ok (state , [])) l in
  ok (state , l)

let fold_map__location_wrap : type a state new_a . (state -> a -> (state * new_a) result) -> state -> a location_wrap -> (state * new_a location_wrap) Simple_utils.Trace.result =
  fun f state { wrap_content ; location } ->
  let%bind ( state , wrap_content ) = f state wrap_content in
  ok (state , ({ wrap_content ; location } : new_a location_wrap))

let fold_map__list_ne : type a state new_a . (state -> a -> (state * new_a) result) -> state -> a list_ne -> (state * new_a list_ne) Simple_utils.Trace.result =
  fun f state (first , l) ->
  let%bind (state , new_first) = f state first in
  let aux acc element =
    let%bind state , l = acc in
    let%bind (state , new_element) = f state element in
    ok (state , new_element :: l) in
  let%bind (state , l) = List.fold_left aux (ok (state , [])) l in
  ok (state , (new_first , l))

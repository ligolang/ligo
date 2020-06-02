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
type z = Z.t
type ligo_string = Stage_common.Types.ligo_string

type constructor' =
| Constructor of string
type label =
| Label of string

module CMap = Map.Make( struct type t = constructor' let compare (Constructor a) (Constructor b) = compare a b end)
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)

type 'a label_map = 'a LMap.t
type 'a constructor_map = 'a CMap.t
type ast_core_type_expression = S.type_expression


type 'a location_wrap = 'a Location.wrap
type 'a list_ne = 'a List.Ne.t
type packed_internal_operation = Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation
type location = Location.t
type inline = bool

type 'a extra_info__comparable = {
  compare : 'a -> 'a -> int ;
}

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

let fold_map__option : type a state new_a . (state -> a -> (state * new_a) result) -> state -> a option -> (state * new_a option) Simple_utils.Trace.result =
  fun f state o ->
  match o with
  | None -> ok (state, None)
  | Some v -> let%bind state, v = f state v in ok (state, Some v)





(* Solver types *)

type 'a poly_unionfind = 'a UnionFind.Poly2.t

(* typevariable: to_string = (fun s -> Format.asprintf "%a" Var.pp s) *)
(* representant for an equivalence class of type variables *)
type 'v typeVariableMap = (type_variable, 'v) RedBlackTrees.PolyMap.t

type 'a poly_set = 'a RedBlackTrees.PolySet.t

let fold_map__poly_unionfind : type a state new_a . new_a extra_info__comparable -> (state -> a -> (state * new_a) result) -> state -> a poly_unionfind -> (state * new_a poly_unionfind) Simple_utils.Trace.result =
  fun extra_info f state l ->
  ignore (extra_info, f, state, l) ; failwith "TODO
  let aux acc element =
    let%bind state , l = acc in
    let%bind (state , new_element) = f state element in ok (state , new_element :: l) in
  let%bind (state , l) = List.fold_left aux (ok (state , [])) l in
  ok (state , l)"

let fold_map__PolyMap : type k v state new_v . (state -> v -> (state * new_v) result) -> state -> (k, v) PolyMap.t -> (state * (k, new_v) PolyMap.t) result =
  fun f state m ->
  let aux k v ~acc =
    let%bind (state , m) = acc in
    let%bind (state , new_v) = f state v in
    ok (state , PolyMap.add k new_v m) in
  let%bind (state , m) = PolyMap.fold_inc aux m ~init:(ok (state, PolyMap.empty m)) in
  ok (state , m)

let fold_map__typeVariableMap : type a state new_a . (state -> a -> (state * new_a) result) -> state -> a typeVariableMap -> (state * new_a typeVariableMap) result =
  fold_map__PolyMap

let fold_map__poly_set : type a state new_a . new_a extra_info__comparable -> (state -> a -> (state * new_a) result) -> state -> a poly_set -> (state * new_a poly_set) result =
  fun extra_info f state s ->
  let new_compare : (new_a -> new_a -> int) = extra_info.compare in
  let aux elt ~acc =
    let%bind (state , s) = acc in
    let%bind (state , new_elt) = f state elt in
    ok (state , PolySet.add new_elt s) in
  let%bind (state , m) = PolySet.fold_inc aux s ~init:(ok (state, PolySet.create ~cmp:new_compare)) in
  ok (state , m)

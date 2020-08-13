module S = Ast_core
open Simple_utils.Trace
include Stage_common.Types

type 'a location_wrap = 'a Location.wrap

type expression_
and expression_variable = expression_ Var.t location_wrap
type type_
and type_variable = type_ Var.t
type z = Z.t
type ast_core_type_expression = S.type_expression


type 'a list_ne = 'a List.Ne.t
type packed_internal_operation = Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation
type location = Location.t
type inline = bool

type 'a extra_info__comparable = {
  compare : 'a -> 'a -> int ;
}

let fold_map__label_map : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a label_map -> (state * new_a label_map , err) result =
  fun f state m ->
  let open LMap in
  let aux k v acc =
    let%bind (state , m) = acc in
    let%bind (state , new_v) = f state v in
    ok (state , add k new_v m) in
  let%bind (state , m) = fold aux m (ok (state, empty)) in
  ok (state , m)

let fold_map__list : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a list -> (state * new_a list , err) result =
  fun f state l ->
  let aux acc element =
    let%bind state , l = acc in
    let%bind (state , new_element) = f state element in ok (state , new_element :: l) in
  let%bind (state , l) = List.fold_left aux (ok (state , [])) l in
  (* fold_left with a list accumulator will produce the results in
     reverse order, so we apply List.rev to put them back in the right
     order. *)
  ok (state , List.rev l)

let fold_map__location_wrap : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a location_wrap -> (state * new_a location_wrap , err) result =
  fun f state { wrap_content ; location } ->
  let%bind ( state , wrap_content ) = f state wrap_content in
  ok (state , ({ wrap_content ; location } : new_a location_wrap))

let fold_map__list_ne : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a list_ne -> (state * new_a list_ne , err) result =
  fun f state (first , l) ->
  let%bind (state , new_first) = f state first in
  let aux acc element =
    let%bind state , l = acc in
    let%bind (state , new_element) = f state element in
    ok (state , new_element :: l) in
  let%bind (state , l) = List.fold_left aux (ok (state , [])) l in
  (* fold_left with a list accumulator will produce the results in
     reverse order, so we apply List.rev to put them back in the right
     order. *)
  ok (state , (new_first , List.rev l))

let fold_map__option : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a option -> (state * new_a option , err) result =
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

let fold_map__poly_unionfind : type a state new_a err . new_a extra_info__comparable -> (state -> a -> (state * new_a, err) result) -> state -> a poly_unionfind -> (state * new_a poly_unionfind, err) result =
  fun extra_info f state l ->
  ignore (extra_info, f, state, l) ; failwith "TODO
  let aux acc element =
    let%bind state , l = acc in
    let%bind (state , new_element) = f state element in ok (state , new_element :: l) in
  let%bind (state , l) = List.fold_left aux (ok (state , [])) l in
  ok (state , l)"

let fold_map__PolyMap : type k v state new_v err . (state -> v -> (state * new_v, err) result) -> state -> (k, v) PolyMap.t -> (state * (k, new_v) PolyMap.t , err) result =
  fun f state m ->
  let aux k v ~acc =
    let%bind (state , m) = acc in
    let%bind (state , new_v) = f state v in
    ok (state , PolyMap.add k new_v m) in
  let%bind (state , m) = PolyMap.fold_inc aux m ~init:(ok (state, PolyMap.empty m)) in
  ok (state , m)

let fold_map__typeVariableMap : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a typeVariableMap -> (state * new_a typeVariableMap , err) result =
  fold_map__PolyMap

let fold_map__poly_set : type a state new_a err . new_a extra_info__comparable -> (state -> a -> (state * new_a, err) result) -> state -> a poly_set -> (state * new_a poly_set, err) result =
  fun extra_info f state s ->
  let new_compare : (new_a -> new_a -> int) = extra_info.compare in
  let aux elt ~acc =
    let%bind (state , s) = acc in
    let%bind (state , new_elt) = f state elt in
    ok (state , PolySet.add new_elt s) in
  let%bind (state , m) = PolySet.fold_inc aux s ~init:(ok (state, PolySet.create ~cmp:new_compare)) in
  ok (state , m)


(* This takes a fold_map__xxx function and turns it into a make__xxx
   function.
   It just swaps the error monad with the option monad, and uses unit
   as the type for the state and for "errors". *)
let fold_map_to_make fold_map = fun f v ->
  match fold_map (fun () x -> match f x with Some x' -> ok ((), x') | None -> Stdlib.Error ()) () v with
    Stdlib.Ok (((), v'), _) -> Some v'
  | Stdlib.Error () -> None

(* This can't be done automatically, because the auto-generated
   comparison functions make use of the fold, the fold supplies to
   users some "make" functions, and there's no deterministic way to
   extract the comparison functions (or other typeclass-like
   functions).

   Instead of writing the following functions, we could just write the
   get_typeclass_compare functions for poly_unionfind and poly_set,
   but the resulting code wouldn't be much clearer. *)
let make__label_map f v = fold_map_to_make fold_map__label_map f v
let make__list f v = fold_map_to_make fold_map__list f v
let make__location_wrap f v = fold_map_to_make fold_map__location_wrap f v
let make__list_ne f v = fold_map_to_make fold_map__list_ne f v
let make__option f v = fold_map_to_make fold_map__option f v
let make__poly_unionfind f v = fold_map_to_make (fold_map__poly_unionfind { compare = failwith "TODO" (*UnionFind.Poly2.get_compare v*) }) f v
let make__PolyMap f v = fold_map_to_make fold_map__PolyMap f v
let make__typeVariableMap f v = fold_map_to_make fold_map__typeVariableMap f v
let make__poly_set f v = fold_map_to_make (fold_map__poly_set { compare = failwith "TODO" (*PolySet.get_compare v*) }) f v

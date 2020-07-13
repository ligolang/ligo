open Trace

type location = Location.t
type 'a location_wrap = 'a Location.wrap
type z = Z.t
type ligo_string = Stage_common.Types.ligo_string
type packed_internal_operation = Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

type expression_
and expression_variable = expression_ Var.t location_wrap
type type_
and type_variable = type_ Var.t

type constructor' =
| Constructor of string
type label =
| Label of string

module CMap = Map.Make( struct type t = constructor' let compare (Constructor a) (Constructor b) = compare a b end)
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)

type 'a label_map = 'a LMap.t
type 'a constructor_map = 'a CMap.t

type sugar_type_expression_option = Ast_sugar.type_expression option
type sugar_expression_option = Ast_sugar.expression option

let fold_map__location_wrap : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a location_wrap -> (state * new_a location_wrap , err) result =
  fun f state { wrap_content ; location } ->
  let%bind ( state , wrap_content ) = f state wrap_content in
  ok (state , ({ wrap_content ; location } : new_a location_wrap))

(* This takes a fold_map__xxx function and turns it into a make__xxx
   function.
   It just swaps the error monad with the option monad, and uses unit
   as the type for the state and for "errors". *)
let fold_map_to_make fold_map = fun f v ->
  match fold_map (fun () x -> match f x with Some x' -> ok ((), x') | None -> Stdlib.Error ()) () v with
    Stdlib.Ok (((), v'), _) -> Some v'
  | Stdlib.Error () -> None

let fold_map__list : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a list -> (state * new_a list , err) result =
  fun f state l ->
  let aux acc element =
    let%bind state , l = acc in
    let%bind (state , new_element) = f state element in ok (state , new_element :: l) in
  let%bind (state , l) = List.fold_left aux (ok (state , [])) l in
  ok (state , l)

let fold_map__constructor_map : type a new_a state err. (state -> a -> (state * new_a, err) result) -> state -> a constructor_map -> (state * new_a constructor_map, err) result =
  fun f state m ->
  let aux k v acc =
    let%bind (state , m) = acc in
    let%bind (state , new_v) = f state v in
    ok (state , CMap.add k new_v m) in
  let%bind (state , m) = CMap.fold aux m (ok (state, CMap.empty)) in
  ok (state , m)

let fold_map__label_map : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a label_map -> (state * new_a label_map , err) result =
  fun f state m ->
  let aux k v acc =
    let%bind (state , m) = acc in
    let%bind (state , new_v) = f state v in
    ok (state , LMap.add k new_v m) in
  let%bind (state , m) = LMap.fold aux m (ok (state, LMap.empty)) in
  ok (state , m)

let fold_map__option : type a state new_a err . (state -> a -> (state * new_a , err) result) -> state -> a option -> (state * new_a option , err) result =
  fun f state o ->
  match o with
  | None -> ok (state, None)
  | Some v -> let%bind state, v = f state v in ok (state, Some v)

(* This can't be done automatically, because the auto-generated
   comparison functions make use of the fold, the fold supplies to
   users some "make" functions, and there's no deterministic way to
   extract the comparison functions (or other typeclass-like
   functions).

   Instead of writing the following functions, we could just write the
   get_typeclass_compare functions for poly_unionfind and poly_set,
   but the resulting code wouldn't be much clearer. *)
let make__location_wrap f v = fold_map_to_make fold_map__location_wrap f v
let make__list f v = fold_map_to_make fold_map__list f v
let make__constructor_map f v = fold_map_to_make fold_map__constructor_map f v
let make__label_map f v = fold_map_to_make fold_map__label_map f v
let make__option f v = fold_map_to_make fold_map__option f v

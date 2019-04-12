open Trace
open Types
open Micheline
open Memory_proto_alpha.Script_ir_translator

module Stack = Meta_michelson.Stack

type element = environment_element

module Small = struct
  open Append_tree

  type t' = environment_small'
  type t = environment_small

  let has' s = exists' (fun ((x, _):element) -> x = s)
  let has s = function
    | Empty -> false
    | Full x -> has' s x

  let empty : t = empty

  let get_opt = assoc_opt

  let append s (e:t) = if has (fst s) e then e else append s e

  let of_list lst =
    let rec aux = function
      | [] -> Empty
      | hd :: tl -> append hd (aux tl)
    in
    aux @@ List.rev lst


  let rec to_list' (e:t') =
    match e with
    | Leaf x -> [x]
    | Node {a;b} -> (to_list' a) @ (to_list' b)

  let to_list (e:t) =
    match e with
    | Empty -> []
    | Full x -> to_list' x

  type bound = string list

  open Michelson

  let rec get_path' = fun s env' ->
    match env' with
    | Leaf (n, v) when n = s -> ok ([], v)
    | Leaf _ -> simple_fail (thunk "Not in env")
    | Node {a;b} ->
        match%bind bind_lr @@ Tezos_utils.Tuple.map2 (get_path' s) (a,b) with
        | `Left (lst, v) -> ok ((`Left :: lst), v)
        | `Right (lst, v) -> ok ((`Right :: lst), v)

  let get_path = fun s env ->
    match env with
    | Empty -> simple_fail (thunk "Set : No env")
    | Full x -> get_path' s x

  let rec to_michelson_get' s = function
    | Leaf (n, tv) when n = s -> ok @@ (seq [], tv)
    | Leaf _ -> simple_fail (thunk "Schema.Small.get : not in env")
    | Node {a;b} -> (
        match%bind bind_lr @@ Tezos_utils.Tuple.map2 (to_michelson_get' s) (a, b) with
        | `Left (x, tv) -> ok @@ (seq [i_car ; x], tv)
        | `Right (x, tv) -> ok @@ (seq [i_cdr ; x], tv)
      )
  let to_michelson_get s = function
    | Empty -> simple_fail (thunk "Schema.Small.get : not in env")
    | Full x -> to_michelson_get' s x

  let rec to_michelson_set' s = function
    | Leaf (n, tv) when n = s -> ok (dip i_drop, tv)
    | Leaf _ -> simple_fail (thunk "Schema.Small.set : not in env")
    | Node {a;b} -> (
        match%bind bind_lr @@ Tezos_utils.Tuple.map2 (to_michelson_set' s) (a, b) with
        | `Left (x, tv) -> ok (seq [dip i_unpair ; x ; i_pair], tv)
        | `Right (x, tv) -> ok (seq [dip i_unpiar ; x ; i_piar], tv)
      )
  let to_michelson_set s = function
    | Empty -> simple_fail (thunk "Schema.Small.set : not in env")
    | Full x -> to_michelson_set' s x

  let rec to_michelson_append' = function
    | Leaf _ -> ok i_piar
    | Node{full=true} -> ok i_piar
    | Node{a=Node _;b;full=false} ->
        let%bind b = to_michelson_append' b in
        ok @@ seq [dip i_unpiar ; b ; i_piar]
    | Node{a=Leaf _;full=false} -> assert false

  let to_michelson_append = function
    | Empty -> ok (dip i_drop)
    | Full x -> to_michelson_append' x

  let rec to_mini_c_capture' env : _ -> expression result = function
    | Leaf (n, tv) -> ok (E_variable n, tv, env)
    | Node {a;b} ->
        let%bind ((_, ty_a, _) as a) = to_mini_c_capture' env a in
        let%bind ((_, ty_b, _) as b) = to_mini_c_capture' env b in
        ok (E_constant ("PAIR", [a;b]), (T_pair(ty_a, ty_b) : type_value), env)

  let to_mini_c_capture env = function
    | Empty -> simple_fail (thunk "to_mini_c_capture")
    | Full x -> to_mini_c_capture' env x

  let rec to_mini_c_type' : _ -> type_value = function
    | Leaf (_, t) -> t
    | Node {a;b} -> T_pair(to_mini_c_type' a, to_mini_c_type' b)

  let to_mini_c_type : _ -> type_value = function
    | Empty -> T_base Base_unit
    | Full x -> to_mini_c_type' x
end

type t = environment

let empty : t = [Small.empty]
let extend t : t = Small.empty :: t
let restrict t : t = List.tl t
let of_small small : t = [small]

let rec get_opt : t -> string -> type_value option = fun t k ->
  match t with
  | [] -> None
  | hd :: tl -> (
      match Small.get_opt hd k with
      | None -> get_opt tl k
      | Some v -> Some v
    )

let rec has x : t -> bool = function
  | [] -> raise (Failure "Schema.Big.has")
  | [hd] -> Small.has x hd
  | hd :: tl -> Small.has x hd || has x tl
let add x : t -> t = function
  | [] -> raise (Failure "Schema.Big.add")
  | hd :: tl -> Small.append x hd :: tl

(* let init_function (f:type_value) (binder:string) : t = [Small.init_function binder f] *)

let to_michelson_extend = Michelson.(
    seq [i_push_unit ; i_pair]
  )
let to_michelson_restrict = Michelson.i_cdr

let to_ty = Compiler_type.Ty.environment
let to_michelson_type = Compiler_type.environment
let rec to_mini_c_type = function
  | [] -> raise (Failure "Schema.Big.to_mini_c_type")
  | [hd] -> Small.to_mini_c_type hd
  | hd :: tl -> T_pair(Small.to_mini_c_type hd, to_mini_c_type tl)
let to_mini_c_capture = function
  | [a] -> Small.to_mini_c_capture a
  | _ -> raise (Failure "Schema.Big.to_mini_c_capture")

let rec get_path : string -> environment -> ([`Left | `Right] list * type_value) result = fun s t ->
  match t with
  | [] -> simple_fail (thunk "Get path : empty big schema")
  | [ x ] -> Small.get_path s x
  | hd :: tl -> (
      match%bind bind_lr_lazy (Small.get_path s hd, (fun () -> get_path s tl)) with
      | `Left (lst, v) -> ok (`Left :: lst, v)
      | `Right (lst, v) -> ok (`Right :: lst, v)
    )

let path_to_michelson_get = fun path ->
  let open Michelson in
  let aux step = match step with
    | `Left -> i_car
    | `Right -> i_cdr in
  seq (List.map aux path)

let path_to_michelson_set = fun path ->
  let open Michelson in
  let aux acc step = match step with
    | `Left -> seq [dip i_unpair ; acc ; i_pair]
    | `Right -> seq [dip i_unpiar ; acc ; i_piar]
  in
  let init = dip i_drop in
  List.fold_left aux init path

let to_michelson_anonymous_add (t:t) =
  let%bind code = match t with
    | [] -> simple_fail (thunk "Schema.Big.Add.to_michelson_add")
    | [hd] -> Small.to_michelson_append hd
    | hd :: _ -> (
        let%bind code = Small.to_michelson_append hd in
        ok @@ Michelson.(seq [dip i_unpair ; code ; i_pair])
      )
  in
  ok code

let to_michelson_add x (t:t) =
  let%bind code = match t with
    | [] -> simple_fail (thunk "Schema.Big.Add.to_michelson_add")
    | [hd] -> Small.to_michelson_append hd
    | hd :: _ -> (
        let%bind code = Small.to_michelson_append hd in
        ok @@ Michelson.(seq [dip i_unpair ; code ; i_pair])
      )
  in

  let%bind _assert_type =
    let new_schema = add x t in
    let%bind (Ex_ty schema_ty) = to_ty t in
    let%bind (Ex_ty new_schema_ty) = to_ty new_schema in
    let%bind (Ex_ty input_ty) = Compiler_type.Ty.type_ (snd x) in
    let input_stack_ty = Stack.(input_ty @: schema_ty @: nil) in
    let output_stack_ty = Stack.(new_schema_ty @: nil) in
    let error_message () = Format.asprintf
        "\nold : %a\nnew : %a\ncode : %a\n"
        PP.environment t
        PP.environment new_schema
        Tezos_utils.Micheline.Michelson.pp code in
    let%bind _ =
      trace_tzresult_lwt (fun () -> error (thunk "error parsing Schema.Big.to_michelson_add code") error_message ()) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty in
    ok ()
  in

  ok code

let to_michelson_get (s:t) str : (Michelson.t * type_value) result =
  let open Michelson in
  let rec aux s str : (Michelson.t * type_value) result  = match s with
    | [] -> simple_fail (thunk "Schema.Big.get")
    | [a] -> Small.to_michelson_get str a
    | a :: b -> (
        match Small.to_michelson_get str a with
        | Trace.Ok (code, tv) -> ok (seq [i_car ; code], tv)
        | Errors _ ->
            let%bind (code, tv) = aux b str in
            ok (seq [i_cdr ; code], tv)
      )
  in
  let%bind (code, tv) = aux s str in

  let%bind _assert_type =
    let%bind (Ex_ty schema_ty) = to_ty s in
    let%bind schema_michelson = to_michelson_type s in
    let%bind (Ex_ty ty) = Compiler_type.Ty.type_ tv in
    let input_stack_ty = Stack.(schema_ty @: nil) in
    let output_stack_ty = Stack.(ty @: nil) in
    let error_message () =
      Format.asprintf
        "\ncode : %a\nschema type : %a"
        Tezos_utils.Micheline.Michelson.pp code
        Tezos_utils.Micheline.Michelson.pp schema_michelson
    in
    let%bind _ =
      trace_tzresult_lwt (fun () -> error (thunk "error parsing big.get code") error_message ()) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty
    in
    ok ()
  in

  ok (code, tv)

let to_michelson_set str (s:t) : Michelson.t result =
  let open Michelson in
  let rec aux s str : (Michelson.t * type_value) result =
    match s with
    | [] -> simple_fail (thunk "Schema.Big.get")
    | [a] -> Small.to_michelson_set str a
    | a :: b -> (
        match Small.to_michelson_set str a with
        | Trace.Ok (code, tv) -> ok (seq [dip i_unpair ; code ; i_pair], tv)
        | Errors _ ->
            let%bind (tmp, tv) = aux b str in
            ok (seq [dip i_unpiar ; tmp ; i_piar], tv)
      )
  in
  let%bind (code, tv) = aux s str in

  let%bind _assert_type =
    let%bind (Ex_ty schema_ty) = to_ty s in
    let%bind schema_michelson = to_michelson_type s in
    let%bind (Ex_ty ty) = Compiler_type.Ty.type_ tv in
    let input_stack_ty = Stack.(ty @: schema_ty @: nil) in
    let output_stack_ty = Stack.(schema_ty @: nil) in
    let error_message () =
      Format.asprintf
        "\ncode : %a\nschema : %a\nschema type : %a"
        Tezos_utils.Micheline.Michelson.pp code
        PP.environment s
        Tezos_utils.Micheline.Michelson.pp schema_michelson
    in
    let%bind _ =
      Trace.trace_tzresult_lwt (fun () -> error (thunk "error parsing big.set code") error_message ()) @@
      Tezos_utils.Memory_proto_alpha.parse_michelson code
        input_stack_ty output_stack_ty
    in
    ok ()
  in

  ok @@ seq [ i_comment "set" ; code ]

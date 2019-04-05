open! Trace
open Tezos_utils.Memory_proto_alpha

open Script_typed_ir
open Script_ir_translator

module Michelson = Tezos_utils.Micheline.Michelson
module Stack = Meta_michelson.Wrap.Stack
module Types = Meta_michelson.Contract.Types
module Append_tree = Tree.Append

type type_name = string

type type_base =
  | Base_unit
  | Base_bool
  | Base_int | Base_nat
  | Base_string | Base_bytes

type type_value =
  | T_pair of (type_value * type_value)
  | T_or of type_value * type_value
  | T_function of type_value * type_value
  | T_deep_closure of environment_small * type_value * type_value
  | T_shallow_closure of environment * type_value * type_value
  | T_base of type_base
  | T_map of (type_value * type_value)
  | T_option of type_value


and environment_element = string * type_value

and environment_small' = environment_element Append_tree.t'

and environment_small = environment_element Append_tree.t

and environment = environment_small list

type environment_wrap = {
  pre_environment : environment ;
  post_environment : environment ;
}

type var_name = string
type fun_name = string

type value =
  | D_unit
  | D_bool of bool
  | D_nat of int
  | D_int of int
  | D_string of string
  | D_bytes of bytes
  | D_pair of value * value
  | D_left of value
  | D_right of value
  | D_some of value
  | D_none
  | D_map of (value * value) list
  (* | `Macro of anon_macro ... The future. *)
  | D_function of anon_function

and expression' =
  | E_literal of value
  | E_function of anon_function_expression
  | E_constant of string * expression list
  | E_application of expression * expression
  | E_variable of var_name
  | E_empty_map of (type_value * type_value)
  | E_make_none of type_value
  | E_Cond of expression * expression * expression

and expression = expression' * type_value * environment (* Environment in which the expressions are evaluated *)

and assignment = var_name * expression

and statement' =
  | Assignment of assignment
  | I_Cond of expression * block * block
  | If_None of expression * block * (var_name * block)
  | While of expression * block

and statement = statement' * environment_wrap

and toplevel_statement = assignment * environment_wrap

and anon_function_content = {
  binder : string ;
  input : type_value ;
  output : type_value ;
  body : block ;
  result : expression ;
  capture_type : capture ;
}

and anon_function = {
  content : anon_function_content ;
  capture : value option ;
}

and anon_function_expression = anon_function_content

and capture =
  | No_capture (* For functions that don't capture their environments. Quotes. *)
  | Shallow_capture of environment (* Duplicates the whole environment. A single DUP. Heavier GETs and SETs at use. *)
  | Deep_capture of environment_small (* Retrieves only the values it needs. Multiple SETs on init. Lighter GETs and SETs at use. *)

and block' = statement list

and block = block' * environment_wrap

and program = toplevel_statement list

module PP = struct
  open Format
  open PP

  let list_sep_d x = list_sep x (const " , ")

  let space_sep ppf () = fprintf ppf " "

  let type_base ppf : type_base -> _ = function
    | Base_unit -> fprintf ppf "unit"
    | Base_bool -> fprintf ppf "bool"
    | Base_int -> fprintf ppf "int"
    | Base_nat -> fprintf ppf "nat"
    | Base_string -> fprintf ppf "string"
    | Base_bytes -> fprintf ppf "bytes"

  let rec type_ ppf : type_value -> _ = function
    | T_or(a, b) -> fprintf ppf "(%a) | (%a)" type_ a type_ b
    | T_pair(a, b) -> fprintf ppf "(%a) & (%a)" type_ a type_ b
    | T_base b -> type_base ppf b
    | T_function(a, b) -> fprintf ppf "(%a) -> (%a)" type_ a type_ b
    | T_map(k, v) -> fprintf ppf "map(%a -> %a)" type_ k type_ v
    | T_option(o) -> fprintf ppf "option(%a)" type_ o
    | T_shallow_closure(_, a, b) -> fprintf ppf "[big_closure](%a) -> (%a)" type_ a type_ b
    | T_deep_closure(c, arg, ret) ->
      fprintf ppf "[%a](%a)->(%a)"
        environment_small c
        type_ arg type_ ret

  and environment_element ppf ((s, tv) : environment_element) =
    Format.fprintf ppf "%s : %a" s type_ tv

  and environment_small' ppf e' = let open Append_tree in
    let lst = to_list' e' in
    fprintf ppf "S[%a]" (list_sep_d environment_element) lst

  and environment_small ppf e = let open Append_tree in
    let lst = to_list e in
    fprintf ppf "S[%a]" (list_sep_d environment_element) lst

  let environment ppf (x:environment) =
    fprintf ppf "Env[%a]" (list_sep_d environment_small) x

  let rec value ppf : value -> unit = function
    | D_bool b -> fprintf ppf "%b" b
    | D_int n -> fprintf ppf "%d" n
    | D_nat n -> fprintf ppf "%d" n
    | D_unit -> fprintf ppf " "
    | D_string s -> fprintf ppf "\"%s\"" s
    | D_bytes _ -> fprintf ppf "[bytes]"
    | D_pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
    | D_left a -> fprintf ppf "L(%a)" value a
    | D_right b -> fprintf ppf "R(%a)" value b
    | D_function x -> function_ ppf x.content
    | D_none -> fprintf ppf "None"
    | D_some s -> fprintf ppf "Some (%a)" value s
    | D_map m -> fprintf ppf "Map[%a]" (list_sep_d value_assoc) m

  and value_assoc ppf : (value * value) -> unit = fun (a, b) ->
    fprintf ppf "%a -> %a" value a value b

  and expression ppf ((e, _, _):expression) = match e with
    | E_variable v -> fprintf ppf "%s" v
    | E_application(a, b) -> fprintf ppf "(%a)@(%a)" expression a expression b
    | E_constant(p, lst) -> fprintf ppf "%s %a" p (pp_print_list ~pp_sep:space_sep expression) lst
    | E_literal v -> fprintf ppf "%a" value v
    | E_function c -> function_ ppf c
    | E_empty_map _ -> fprintf ppf "map[]"
    | E_make_none _ -> fprintf ppf "none"
    | E_Cond (c, a, b) -> fprintf ppf "%a ? %a : %a" expression c expression a expression b

  and function_ ppf ({binder ; input ; output ; body ; result}:anon_function_content) =
        fprintf ppf "fun (%s:%a) : %a %a return %a"
          binder
          type_ input
          type_ output
          block body
          expression result

  and assignment ppf ((n, e):assignment) = fprintf ppf "let %s = %a;" n expression e

  and statement ppf ((s, _) : statement) = match s with
    | Assignment ass -> assignment ppf ass
    | I_Cond (expr, i, e) -> fprintf ppf "if (%a) %a %a" expression expr block i block e
    | If_None (expr, none, (name, some)) -> fprintf ppf "if (%a) %a %s.%a" expression expr block none name block some
    | While (e, b) -> fprintf ppf "while (%a) %a" expression e block b

  and block ppf ((b, _):block) =
    fprintf ppf "@[<v 2>{@,%a@]@,}" (pp_print_list ~pp_sep:pp_print_newline statement) b

  let tl_statement ppf (ass, _) = assignment ppf ass

  let program ppf (p:program) =
    fprintf ppf "Program:\n---\n%a" (pp_print_list ~pp_sep:pp_print_newline tl_statement) p
end

module Translate_type = struct
  module O = Tezos_utils.Micheline.Michelson

  module Ty = struct

    let not_comparable name = error "not a comparable type" name

    let comparable_type_base : type_base -> ex_comparable_ty result = fun tb ->
      let open Types in
      let return x = ok @@ Ex_comparable_ty x in
      match tb with
      | Base_unit -> fail (not_comparable "unit")
      | Base_bool -> fail (not_comparable "bool")
      | Base_nat -> return nat_k
      | Base_int -> return int_k
      | Base_string -> return string_k
      | Base_bytes -> return bytes_k

    let comparable_type : type_value -> ex_comparable_ty result = fun tv ->
      match tv with
      | T_base b -> comparable_type_base b
      | T_deep_closure _ -> fail (not_comparable "deep closure")
      | T_shallow_closure _ -> fail (not_comparable "shallow closure")
      | T_function _ -> fail (not_comparable "function")
      | T_or _ -> fail (not_comparable "or")
      | T_pair _ -> fail (not_comparable "pair")
      | T_map _ -> fail (not_comparable "map")
      | T_option _ -> fail (not_comparable "option")

    let base_type : type_base -> ex_ty result = fun b ->
      let open Types in
      let return x = ok @@ Ex_ty x in
      match b with
      | Base_unit -> return unit
      | Base_bool -> return bool
      | Base_int -> return int
      | Base_nat -> return nat
      | Base_string -> return string
      | Base_bytes -> return bytes


    let rec type_ : type_value -> ex_ty result =
      function
      | T_base b -> base_type b
      | T_pair (t, t') -> (
          type_ t >>? fun (Ex_ty t) ->
          type_ t' >>? fun (Ex_ty t') ->
          ok @@ Ex_ty (Types.pair t t')
        )
      | T_or (t, t') -> (
          type_ t >>? fun (Ex_ty t) ->
          type_ t' >>? fun (Ex_ty t') ->
          ok @@ Ex_ty (Types.union t t')
        )
      | T_function (arg, ret) ->
          let%bind (Ex_ty arg) = type_ arg in
          let%bind (Ex_ty ret) = type_ ret in
          ok @@ Ex_ty (Types.lambda arg ret)
      | T_deep_closure (c, arg, ret) ->
          let%bind (Ex_ty capture) = environment_small c in
          let%bind (Ex_ty arg) = type_ arg in
          let%bind (Ex_ty ret) = type_ ret in
          ok @@ Ex_ty Types.(pair capture @@ lambda (pair capture arg) ret)
      | T_shallow_closure (c, arg, ret) ->
          let%bind (Ex_ty capture) = environment c in
          let%bind (Ex_ty arg) = type_ arg in
          let%bind (Ex_ty ret) = type_ ret in
          ok @@ Ex_ty Types.(pair capture @@ lambda (pair capture arg) ret)
      | T_map (k, v) ->
          let%bind (Ex_comparable_ty k') = comparable_type k in
          let%bind (Ex_ty v') = type_ v in
          ok @@ Ex_ty Types.(map k' v')
      | T_option t ->
          let%bind (Ex_ty t') = type_ t in
          ok @@ Ex_ty Types.(option t')


    and environment_small' = let open Append_tree in function
      | Leaf (_, x) -> type_ x
      | Node {a;b} ->
        let%bind (Ex_ty a) = environment_small' a in
        let%bind (Ex_ty b) = environment_small' b in
        ok @@ Ex_ty (Types.pair a b)

    and environment_small = function
      | Empty -> ok @@ Ex_ty Types.unit
      | Full x -> environment_small' x

    and environment = function
      | [] -> simple_fail "Schema.Big.to_ty"
      | [a] -> environment_small a
      | a::b ->
        let%bind (Ex_ty a) = environment_small a in
        let%bind (Ex_ty b) = environment b in
        ok @@ Ex_ty (Types.pair a b)
  end


  let base_type : type_base -> O.michelson result =
    function
    | Base_unit -> ok @@ O.prim T_unit
    | Base_bool -> ok @@ O.prim T_bool
    | Base_int -> ok @@ O.prim T_int
    | Base_nat -> ok @@ O.prim T_nat
    | Base_string -> ok @@ O.prim T_string
    | Base_bytes -> ok @@ O.prim T_bytes

  let rec type_ : type_value -> O.michelson result =
    function
    | T_base b -> base_type b
    | T_pair (t, t') -> (
        type_ t >>? fun t ->
        type_ t' >>? fun t' ->
        ok @@ O.prim ~children:[t;t'] O.T_pair
      )
    | T_or (t, t') -> (
        type_ t >>? fun t ->
        type_ t' >>? fun t' ->
        ok @@ O.prim ~children:[t;t'] O.T_or
      )
    | T_map kv ->
        let%bind (k', v') = bind_map_pair type_ kv in
        ok @@ O.prim ~children:[k';v'] O.T_map
    | T_option o ->
        let%bind o' = type_ o in
        ok @@ O.prim ~children:[o'] O.T_option
    | T_function (arg, ret) ->
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ O.prim ~children:[arg;ret] T_lambda
    | T_deep_closure (c, arg, ret) ->
      let%bind capture = environment_small c in
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ O.t_pair capture (O.t_lambda (O.t_pair capture arg) ret)
    | T_shallow_closure (c, arg, ret) ->
      let%bind capture = environment c in
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ O.t_pair capture (O.t_lambda (O.t_pair capture arg) ret)

  and environment_element (name, tyv) =
    let%bind michelson_type = type_ tyv in
    ok @@ O.annotate ("@" ^ name) michelson_type

  and environment_small' = let open Append_tree in function
    | Leaf x -> environment_element x
    | Node {a;b} ->
      let%bind a = environment_small' a in
      let%bind b = environment_small' b in
      ok @@ O.t_pair a b

  and environment_small = function
    | Empty -> ok @@ O.prim O.T_unit
    | Full x -> environment_small' x

  and environment =
    function
    | [] -> simple_fail "Schema.Big.to_michelson_type"
    | [a] -> environment_small a
    | a :: b ->
      let%bind a = environment_small a in
      let%bind b = environment b in
      ok @@ O.t_pair a b

end

module Environment = struct
  open Tezos_utils.Micheline

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

    let rec to_michelson_get' s = function
      | Leaf (n, tv) when n = s -> ok @@ (seq [], tv)
      | Leaf _ -> simple_fail "Schema.Small.get : not in env"
      | Node {a;b} -> (
          match%bind bind_lr @@ Tezos_utils.Tuple.map2 (to_michelson_get' s) (a, b) with
          | `Left (x, tv) -> ok @@ (seq [i_car ; x], tv)
          | `Right (x, tv) -> ok @@ (seq [i_cdr ; x], tv)
        )
    let to_michelson_get s = function
      | Empty -> simple_fail "Schema.Small.get : not in env"
      | Full x -> to_michelson_get' s x

    let rec to_michelson_set' s = function
      | Leaf (n, tv) when n = s -> ok (dip i_drop, tv)
      | Leaf _ -> simple_fail "Schema.Small.set : not in env"
      | Node {a;b} -> (
          match%bind bind_lr @@ Tezos_utils.Tuple.map2 (to_michelson_set' s) (a, b) with
          | `Left (x, tv) -> ok (seq [dip i_unpair ; x ; i_pair], tv)
          | `Right (x, tv) -> ok (seq [dip i_unpiar ; x ; i_piar], tv)
        )
    let to_michelson_set s = function
      | Empty -> simple_fail "Schema.Small.set : not in env"
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
      | Empty -> simple_fail "to_mini_c_capture"
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

  let to_ty = Translate_type.Ty.environment
  let to_michelson_type = Translate_type.environment
  let rec to_mini_c_type = function
    | [] -> raise (Failure "Schema.Big.to_mini_c_type")
    | [hd] -> Small.to_mini_c_type hd
    | hd :: tl -> T_pair(Small.to_mini_c_type hd, to_mini_c_type tl)
  let to_mini_c_capture = function
    | [a] -> Small.to_mini_c_capture a
    | _ -> raise (Failure "Schema.Big.to_mini_c_capture")

  let to_michelson_anonymous_add (t:t) =
    let%bind code = match t with
      | [] -> simple_fail "Schema.Big.Add.to_michelson_add"
      | [hd] -> Small.to_michelson_append hd
      | hd :: _ -> (
          let%bind code = Small.to_michelson_append hd in
          ok @@ Michelson.(seq [dip i_unpair ; code ; i_pair])
        )
    in
    ok code

  let to_michelson_add x (t:t) =
    let%bind code = match t with
      | [] -> simple_fail "Schema.Big.Add.to_michelson_add"
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
      let%bind (Ex_ty input_ty) = Translate_type.Ty.type_ (snd x) in
      let input_stack_ty = Stack.(input_ty @: schema_ty @: nil) in
      let output_stack_ty = Stack.(new_schema_ty @: nil) in
      let error_message = Format.asprintf
          "\nold : %a\nnew : %a\ncode : %a\n"
          PP.environment t
          PP.environment new_schema
          Tezos_utils.Micheline.Michelson.pp code in
      let%bind _ =
        trace_tzresult_lwt (error "error parsing Schema.Big.to_michelson_add code" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson code
          input_stack_ty output_stack_ty in
      ok ()
    in

    ok code

  let to_michelson_get (s:t) str : (Michelson.t * type_value) result =
    let open Michelson in
    let rec aux s str : (Michelson.t * type_value) result  = match s with
      | [] -> simple_fail "Schema.Big.get"
      | [a] -> Small.to_michelson_get str a
      | a :: b -> (
          match Small.to_michelson_get str a with
          | Ok (code, tv) -> ok (seq [i_car ; code], tv)
          | Errors _ ->
            let%bind (code, tv) = aux b str in
            ok (seq [i_car ; code], tv)
        )
    in
    let%bind (code, tv) = aux s str in

    let%bind _assert_type =
      let%bind (Ex_ty schema_ty) = to_ty s in
      let%bind schema_michelson = to_michelson_type s in
      let%bind (Ex_ty ty) = Translate_type.Ty.type_ tv in
      let input_stack_ty = Stack.(schema_ty @: nil) in
      let output_stack_ty = Stack.(ty @: nil) in
      let%bind error_message =
        ok @@ Format.asprintf
          "\ncode : %a\nschema type : %a"
          Tezos_utils.Micheline.Michelson.pp code
          Tezos_utils.Micheline.Michelson.pp schema_michelson
      in
      let%bind _ =
        trace_tzresult_lwt (error "error parsing big.get code" error_message) @@
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
      | [] -> simple_fail "Schema.Big.get"
      | [a] -> Small.to_michelson_set str a
      | a :: b -> (
          match Small.to_michelson_set str a with
          | Ok (code, tv) -> ok (seq [dip i_unpair ; code ; i_pair], tv)
          | Errors _ ->
              let%bind (tmp, tv) = aux b str in
              ok (seq [dip i_unpiar ; tmp ; i_piar], tv)
        )
    in
    let%bind (code, tv) = aux s str in

    let%bind _assert_type =
      let%bind (Ex_ty schema_ty) = to_ty s in
      let%bind schema_michelson = to_michelson_type s in
      let%bind (Ex_ty ty) = Translate_type.Ty.type_ tv in
      let input_stack_ty = Stack.(ty @: schema_ty @: nil) in
      let output_stack_ty = Stack.(schema_ty @: nil) in
      let%bind error_message =
        ok @@ Format.asprintf
          "\ncode : %a\nschema : %a\nschema type : %a"
          Tezos_utils.Micheline.Michelson.pp code
          PP.environment s
          Tezos_utils.Micheline.Michelson.pp schema_michelson
      in
      let%bind _ =
        Trace.trace_tzresult_lwt (error "error parsing big.set code" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson code
          input_stack_ty output_stack_ty
      in
      ok ()
    in

    ok code
end

module Translate_program = struct
  open Tezos_utils.Micheline.Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson

  let simple_constant c = Constant ( seq [
      c ; i_pair ;
    ])

  let simple_unary c = Unary ( seq [
      i_unpair ; c ; i_pair ;
    ])

  let simple_binary c = Binary ( seq [
      i_unpair ; dip i_unpair ; i_swap ; c ; i_pair ;
    ])

  let simple_ternary c = Ternary ( seq [
      i_unpair ; dip i_unpair ; dip (dip i_unpair) ; i_swap ; dip i_swap ; i_swap ; c ; i_pair ;
    ])

  let rec get_predicate : string -> predicate result = function
    | "ADD_INT" -> ok @@ simple_binary @@ prim I_ADD
    | "NEG" -> ok @@ simple_unary @@ prim I_NEG
    | "OR" -> ok @@ simple_binary @@ prim I_OR
    | "AND" -> ok @@ simple_binary @@ prim I_AND
    | "PAIR" -> ok @@ simple_binary @@ prim I_PAIR
    | "CAR" -> ok @@ simple_unary @@ prim I_CAR
    | "CDR" -> ok @@ simple_unary @@ prim I_CDR
    | "EQ" -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_EQ]
    | "UPDATE" -> ok @@ simple_ternary @@ prim I_UPDATE
    | "SOME" -> ok @@ simple_unary @@ prim I_SOME
    | "GET_FORCE" -> ok @@ simple_binary @@ seq [prim I_GET ; i_assert_some]
    | "GET" -> ok @@ simple_binary @@ prim I_GET
    | x -> simple_fail @@ "predicate \"" ^ x ^ "\" doesn't exist"

  and translate_value (v:value) : michelson result = match v with
    | D_bool b -> ok @@ prim (if b then D_True else D_False)
    | D_int n -> ok @@ int (Z.of_int n)
    | D_nat n -> ok @@ int (Z.of_int n)
    | D_string s -> ok @@ string s
    | D_bytes s -> ok @@ bytes (Tezos_stdlib.MBytes.of_bytes s)
    | D_unit -> ok @@ prim D_Unit
    | D_pair (a, b) -> (
        let%bind a = translate_value a in
        let%bind b = translate_value b in
        ok @@ prim ~children:[a;b] D_Pair
      )
    | D_left a -> translate_value a >>? fun a -> ok @@ prim ~children:[a] D_Left
    | D_right b -> translate_value b >>? fun b -> ok @@ prim ~children:[b] D_Right
    | D_function anon -> translate_function anon
    | D_none -> ok @@ prim D_None
    | D_some s ->
        let%bind s' = translate_value s in
        ok @@ prim ~children:[s'] D_Some
    | D_map lst ->
        let%bind lst' = bind_map_list (bind_map_pair translate_value) lst in
        let aux (a, b) = prim ~children:[a;b] D_Elt in
        ok @@ seq @@ List.map aux lst'

  and translate_function ({capture;content}:anon_function) : michelson result =
    let {capture_type } = content in
    match capture, capture_type with
    | _, No_capture ->
        let%bind body = translate_function_body content in
        ok @@ seq [ body ]
    | Some value, Deep_capture _ -> (
        let%bind body = translate_function_body content in
        let%bind capture_m = translate_value value in
        ok @@ d_pair capture_m body
      )
    | Some value, Shallow_capture _ ->
        let%bind body = translate_function_body content in
        let%bind capture_m = translate_value value in
        ok @@ d_pair capture_m body
    | _ -> simple_fail "translating closure without capture"

  and translate_expression ((expr', ty, env) as expr:expression) : michelson result =
    let error_message = Format.asprintf  "%a" PP.expression expr in
    let%bind (code : michelson) = trace (error "translating expression" error_message) @@ match expr' with
      | E_literal v ->
        let%bind v = translate_value v in
        let%bind t = Translate_type.type_ ty in
        ok @@ seq [
          prim ~children:[t;v] I_PUSH ;
          prim I_PAIR ;
        ]
      | E_application((_, f_ty, _) as f, arg) -> (
          match f_ty with
          | T_function _ -> (
              let%bind f = translate_expression f in
              let%bind arg = translate_expression arg in
              ok @@ seq [
                arg ;
                i_unpair ;
                dip f ;
                dip i_unpair ;
                prim I_EXEC ;
                i_pair ;
              ]
            )
          | T_deep_closure (small_env, _, _) -> (
              let env' = Environment.of_small small_env in
              let%bind add = Environment.to_michelson_anonymous_add env' in
              let%bind f = translate_expression f in
              let%bind arg = translate_expression arg in
              ok @@ seq [
                f ; i_unpair ; (* closure :: expr :: env *)
                dip arg ; dip i_unpair ; (* closure :: arg :: expr :: env *)
                i_unpair ; dip add ; (* fun :: full_arg :: expr :: env *)
                i_swap ; prim I_EXEC ;
                i_pair ; (* expr :: env *)
              ]
            )
          | T_shallow_closure (env', _, _) -> (
              let%bind add = Environment.to_michelson_anonymous_add env' in
              let%bind f = translate_expression f in
              let%bind arg = translate_expression arg in
              ok @@ seq [
                f ; i_unpair ; (* closure :: expr :: env *)
                dip arg ; dip i_unpair ; (* closure :: arg :: expr :: env *)
                i_unpair ; dip add ; (* fun :: full_arg :: expr :: env *)
                i_swap ; prim I_EXEC ;
                i_pair ; (* expr :: env *)
              ]
            )
          | _ -> simple_fail "E_applicationing something not appliable"
        )
      | E_variable x ->
        let%bind (get, _) = Environment.to_michelson_get env x in
        ok @@ seq [
          dip (seq [prim I_DUP ; get]) ;
          i_piar ;
        ]
      | E_constant(str, lst) ->
        let%bind lst = bind_list @@ List.map translate_expression lst in
        let%bind predicate = get_predicate str in
        let%bind code = match (predicate, List.length lst) with
          | Constant c, 0 -> ok (seq @@ lst @ [c])
          | Unary f, 1 -> ok (seq @@ lst @ [f])
          | Binary f, 2 -> ok (seq @@ lst @ [f])
          | Ternary f, 3 -> ok (seq @@ lst @ [f])
          | _ -> simple_fail "bad arity"
        in
        ok code
      | E_empty_map sd ->
          let%bind (src, dst) = bind_map_pair Translate_type.type_ sd in
          let code = seq [
              prim ~children:[src;dst] I_EMPTY_MAP ;
              i_pair ;
            ] in
          ok code
      | E_make_none o ->
          let%bind o' = Translate_type.type_ o in
          let code = seq [
              prim ~children:[o'] I_NONE ;
              i_pair ;
            ] in
          ok code
      | E_function anon -> (
          match ty with
          | T_function (_, _) ->
              let%bind body = translate_function_body anon in
              let%bind input_type = Translate_type.type_ anon.input in
              let%bind output_type = Translate_type.type_ anon.output in
              let code = seq [
                  i_lambda input_type output_type body ;
                  i_pair ;
                ] in
              ok code
          | T_deep_closure (small_env, _, _) ->
              (* Capture the variable bounds, assemble them. On call, append the input. *)
              let%bind body = translate_function_body anon in
              let%bind capture = Environment.Small.to_mini_c_capture env small_env in
              let%bind capture = translate_expression capture in
              let%bind input_type = Translate_type.type_ anon.input in
              let%bind output_type = Translate_type.type_ anon.output in
              let code = seq [
                  capture ;
                  i_unpair ;
                  i_lambda input_type output_type body ;
                  i_piar ;
                  i_pair ;
                ] in
              ok code
          | T_shallow_closure (_, _, _) ->
              (* Capture the whole environment. *)
              let%bind body = translate_function_body anon in
              let%bind input_type = Translate_type.type_ anon.input in
              let%bind output_type = Translate_type.type_ anon.output in
              let code = seq [
                  dip i_dup ; i_swap ;
                  i_lambda input_type output_type body ;
                  i_piar ;
                  i_pair ;
                ] in
              ok code
          | _ -> simple_fail "expected function code"
        )
      | E_Cond (c, a, b) -> (
          let%bind c' = translate_expression c in
          let%bind a' = translate_expression a in
          let%bind b' = translate_expression b in
          let%bind code = ok (seq [
              c' ; i_unpair ;
              i_if a' b' ;
            ]) in
          ok code
        )
    in

    let%bind () =
      let%bind (Ex_ty schema_ty) = Environment.to_ty env in
      let%bind output_type = Translate_type.type_ ty in
      let%bind (Ex_ty output_ty) =
        let error_message = Format.asprintf "%a" Michelson.pp output_type in
        Trace.trace_tzresult_lwt (error "error parsing output ty" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson_ty output_type in
      let input_stack_ty = Stack.(Types.unit @: schema_ty @: nil) in
      let output_stack_ty = Stack.(Types.(pair output_ty unit) @: schema_ty @: nil) in
      let%bind error_message =
        let%bind schema_michelson = Environment.to_michelson_type env in
        ok @@ Format.asprintf
          "expression : %a\ncode : %a\nschema type : %a\noutput type : %a"
          PP.expression expr
          Michelson.pp code
          Michelson.pp schema_michelson
          Michelson.pp output_type
      in
      let%bind _ =
        Trace.trace_tzresult_lwt (error "error parsing expression code" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson code
          input_stack_ty output_stack_ty
      in
      ok ()
    in
    ok code

  and translate_statement ((s', w_env) as s:statement) : michelson result =
    let error_message = Format.asprintf "%a" PP.statement s in
    let%bind (code : michelson) =
      trace (error "translating statement" error_message) @@ match s' with
    | Assignment (s, ((_, tv, _) as expr)) ->
      let%bind expr = translate_expression expr in
      let%bind add =
        if Environment.has s w_env.pre_environment
        then Environment.to_michelson_set s w_env.pre_environment
        else Environment.to_michelson_add (s, tv) w_env.pre_environment
      in
      ok (seq [
          i_comment "assignment" ;
          seq [
            i_comment "expr" ;
            i_push_unit ; expr ; i_car ;
          ] ;
          seq [
            i_comment "env <- env . expr" ;
            add ;
          ];
        ])
    | I_Cond (expr, a, b) ->
      let%bind expr = translate_expression expr in
      let%bind a = translate_regular_block a in
      let%bind b = translate_regular_block b in
      ok @@ (seq [
          i_push_unit ;
          expr ;
          prim I_CAR ;
          dip Environment.to_michelson_extend ;
          prim ~children:[seq [a ; Environment.to_michelson_restrict];seq [b ; Environment.to_michelson_restrict]] I_IF ;
        ])
    | If_None (expr, none, (_, some)) ->
        let%bind expr = translate_expression expr in
        let%bind none' = translate_regular_block none in
        let%bind some' = translate_regular_block some in
        let%bind add =
          let env = Environment.extend w_env.pre_environment in
          Environment.to_michelson_anonymous_add env in
        ok @@ (seq [
            i_push_unit ; expr ; i_car ;
            dip Environment.to_michelson_extend ;
            prim ~children:[
              seq [none' ; Environment.to_michelson_restrict] ;
              seq [add ; some' ; Environment.to_michelson_restrict] ;
            ] I_IF_NONE
          ])
    | While ((_, _, _) as expr, block) ->
      let%bind expr = translate_expression expr in
      let%bind block = translate_regular_block block in
      ok @@ (seq [
        i_push_unit ; expr ; i_car ;
        dip Environment.to_michelson_extend ;
        prim ~children:[block ; Environment.to_michelson_restrict ; i_push_unit ; expr ; i_car] I_LOOP ;
      ])
    in
    ok code

  and translate_regular_block ((b, env):block) : michelson result =
    let aux prev statement =
      let%bind (lst : michelson list) = prev in
      let%bind instruction = translate_statement statement in
      ok (instruction :: lst)
    in
    let%bind error_message =
      let%bind schema_michelson = Environment.to_michelson_type env.pre_environment in
      ok @@ Format.asprintf "\nblock : %a\nschema : %a\n"
        PP.block (b, env)
        Tezos_utils.Micheline.Michelson.pp schema_michelson
    in
    let%bind codes =
      trace (error "error translating block" error_message) @@
      List.fold_left aux (ok []) b in
    let code = seq (List.rev codes) in
    ok code

  and translate_function_body ({body;result} as f:anon_function_content) : michelson result =
    let%bind body = translate_regular_block body in
    let%bind expr = translate_expression result in
    let code = seq [
        body ;
        i_push_unit ; expr ; i_car ;
        dip i_drop ;
      ] in

    let%bind _assert_type =
      let%bind (Ex_ty input_ty) = Translate_type.Ty.type_ f.input in
      let%bind (Ex_ty output_ty) = Translate_type.Ty.type_ f.output in
      let input_stack_ty = Stack.(input_ty @: nil) in
      let output_stack_ty = Stack.(output_ty @: nil) in
      let%bind error_message =
        ok @@ Format.asprintf
          "\ncode : %a\n"
          Tezos_utils.Micheline.Michelson.pp code
      in
      let%bind _ =
        Trace.trace_tzresult_lwt (error "error parsing function code" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson code
          input_stack_ty output_stack_ty
      in
      ok ()
    in

    ok code

  type compiled_program = {
    input : ex_ty ;
    output : ex_ty ;
    body : michelson ;
  }

  let translate_program (p:program) (entry:string) : compiled_program result =
    let is_main ((s, _):toplevel_statement) = match s with
      | name , (E_function f, T_function (_, _), _) when f.capture_type = No_capture && name = entry -> Some f
      | _ -> None in
    let%bind main =
      trace_option (simple_error "no functional entry") @@
      Tezos_utils.List.find_map is_main p in
    let {input;output} : anon_function_content = main in
    let%bind body = translate_function_body main in
    let%bind input = Translate_type.Ty.type_ input in
    let%bind output = Translate_type.Ty.type_ output in
    ok ({input;output;body}:compiled_program)

  let translate_entry (p:anon_function) : compiled_program result =
    let {input;output} : anon_function_content = p.content in
    let%bind body = translate_function_body p.content in
    let%bind input = Translate_type.Ty.type_ input in
    let%bind output = Translate_type.Ty.type_ output in
    ok ({input;output;body}:compiled_program)

end

module Translate_ir = struct

  let rec translate_value (Ex_typed_value (ty, value)) : value result =
    match (ty, value) with
    | Pair_t ((a_ty, _, _), (b_ty, _, _), _), (a, b) -> (
        let%bind a = translate_value @@ Ex_typed_value(a_ty, a) in
        let%bind b = translate_value @@ Ex_typed_value(b_ty, b) in
        ok @@ D_pair(a, b)
      )
    | Union_t ((a_ty, _), _, _), L a -> (
        let%bind a = translate_value @@ Ex_typed_value(a_ty, a) in
        ok @@ D_left a
      )
    | Union_t (_, (b_ty, _), _), R b -> (
        let%bind b = translate_value @@ Ex_typed_value(b_ty, b) in
        ok @@ D_right b
      )
    | (Int_t _), n ->
        let%bind n =
          trace_option (simple_error "too big to fit an int") @@
          Alpha_context.Script_int.to_int n in
        ok @@ D_int n
    | (Nat_t _), n ->
        let%bind n =
          trace_option (simple_error "too big to fit an int") @@
          Alpha_context.Script_int.to_int n in
        ok @@ D_nat n
    | (Bool_t _), b ->
        ok @@ D_bool b
    | (Unit_t _), () ->
        ok @@ D_unit
    | (Option_t _), None ->
        ok @@ D_none
    | (Option_t ((o_ty, _), _, _)), Some s ->
        let%bind s' = translate_value @@ Ex_typed_value (o_ty, s) in
        ok @@ D_some s'
    | (Map_t (k_cty, v_ty, _)), m ->
        let k_ty = Script_ir_translator.ty_of_comparable_ty k_cty in
        let lst =
          let aux k v acc = (k, v) :: acc in
          let lst = Script_ir_translator.map_fold aux m [] in
          List.rev lst in
        let%bind lst' =
          let aux (k, v) =
            let%bind k' = translate_value (Ex_typed_value (k_ty, k)) in
            let%bind v' = translate_value (Ex_typed_value (v_ty, v)) in
            ok (k', v')
          in
          bind_map_list aux lst
        in
        ok @@ D_map lst'
    | _ -> simple_fail "this value can't be transpiled back yet"
end


module Run = struct

  open Tezos_utils.Micheline
  open! Translate_program

  let run_aux (program:compiled_program) (input_michelson:Michelson.t) : ex_typed_value result =
    let open Meta_michelson.Wrap in
    let {input;output;body} : compiled_program = program in
    let (Ex_ty input_ty) = input in
    let (Ex_ty output_ty) = output in
    let%bind input =
      Trace.trace_tzresult_lwt (simple_error "error parsing input") @@
      Tezos_utils.Memory_proto_alpha.parse_michelson_data input_michelson input_ty in
    let body = Michelson.strip_annots body in
    let%bind descr =
      Trace.trace_tzresult_lwt (simple_error "error parsing program code") @@
      Tezos_utils.Memory_proto_alpha.parse_michelson body
        (Stack.(input_ty @: nil)) (Stack.(output_ty @: nil)) in
    let open! Script_interpreter in
    let%bind (Item(output, Empty)) =
      Trace.trace_tzresult_lwt (simple_error "error of execution") @@
      Tezos_utils.Memory_proto_alpha.interpret descr (Item(input, Empty)) in
    ok (Ex_typed_value (output_ty, output))

  let run_node (program:program) (input:Michelson.t) : Michelson.t result =
    let%bind compiled = translate_program program "main" in
    let%bind (Ex_typed_value (output_ty, output)) = run_aux compiled input in
    let%bind output =
      Trace.trace_tzresult_lwt (simple_error "error unparsing output") @@
      Tezos_utils.Memory_proto_alpha.unparse_michelson_data output_ty output in
    ok output

  let run_entry (entry:anon_function) (input:value) : value result =
    let%bind compiled = translate_entry entry in
    let%bind input_michelson = translate_value input in
    let%bind ex_ty_value = run_aux compiled input_michelson in
    let%bind (result : value) = Translate_ir.translate_value ex_ty_value in
    ok result

  let run (program:program) (input:value) : value result =
    let%bind input_michelson = translate_value input in
    let%bind compiled = translate_program program "main" in
    let%bind ex_ty_value = run_aux compiled input_michelson in
    let%bind (result : value) = Translate_ir.translate_value ex_ty_value in
    ok result


  let expression_to_value ((e', _, _) as e:expression) : value result =
    match e' with
    | E_literal v -> ok v
    | _ -> fail
        @@ error "not a value"
        @@ Format.asprintf "%a" PP.expression e

end


module Combinators = struct

  let get_bool (v:value) = match v with
    | D_bool b -> ok b
    | _ -> simple_fail "not a bool"

  let get_int (v:value) = match v with
    | D_int n -> ok n
    | _ -> simple_fail "not an int"

  let get_string (v:value) = match v with
    | D_string s -> ok s
    | _ -> simple_fail "not a string"

  let get_bytes (v:value) = match v with
    | D_bytes b -> ok b
    | _ -> simple_fail "not a bytes"

  let get_unit (v:value) = match v with
    | D_unit -> ok ()
    | _ -> simple_fail "not a unit"

  let get_option (v:value) = match v with
    | D_none -> ok None
    | D_some s -> ok (Some s)
    | _ -> simple_fail "not an option"

  let get_map (v:value) = match v with
    | D_map lst -> ok lst
    | _ -> simple_fail "not a map"

  let get_t_option (v:type_value) = match v with
    | T_option t -> ok t
    | _ -> simple_fail "not an option"

  let get_pair (v:value) = match v with
    | D_pair (a, b) -> ok (a, b)
    | _ -> simple_fail "not a pair"

  let get_t_pair (t:type_value) = match t with
    | T_pair (a, b) -> ok (a, b)
    | _ -> simple_fail "not a type pair"

  let get_t_map (t:type_value) = match t with
    | T_map kv -> ok kv
    | _ -> simple_fail "not a type map"

  let get_left (v:value) = match v with
    | D_left b -> ok b
    | _ -> simple_fail "not a left"

  let get_right (v:value) = match v with
    | D_right b -> ok b
    | _ -> simple_fail "not a right"

  let get_or (v:value) = match v with
    | D_left b -> ok (false, b)
    | D_right b -> ok (true, b)
    | _ -> simple_fail "not a left/right"

  let get_last_statement ((b', _):block) : statement result =
    let aux lst = match lst with
      | [] -> simple_fail "get_last: empty list"
      | lst -> ok List.(nth lst (length lst - 1)) in
    aux b'

  let t_int : type_value = T_base Base_int

  let quote binder input output body result : anon_function =
    let content : anon_function_content = {
      binder ; input ; output ;
      body ; result ; capture_type = No_capture ;
    } in
    { content ; capture = None }

  let basic_quote i o b : anon_function result =
    let%bind (_, e) = get_last_statement b in
    let r : expression = (E_variable "output", o, e.post_environment) in
    ok @@ quote "input" i o b r

  let basic_int_quote b : anon_function result =
    basic_quote t_int t_int b

  let basic_int_quote_env : environment =
    let e = Environment.empty in
    Environment.add ("input", t_int) e

  let e_int expr env : expression = (expr, t_int, env)
  let e_var_int name env : expression = e_int (E_variable name) env

  let d_unit : value = D_unit

  let environment_wrap pre_environment post_environment = { pre_environment ; post_environment }
  let id_environment_wrap e = environment_wrap e e

  let statement s' e : statement =
    match s' with
    | I_Cond _ -> s', id_environment_wrap e
    | If_None _ -> s', id_environment_wrap e
    | While _ -> s', id_environment_wrap e
    | Assignment (name, (_, t, _)) -> s', environment_wrap e (Environment.add (name, t) e)

  let block (statements:statement list) : block result =
    match statements with
    | [] -> simple_fail "no statements in block"
    | lst ->
        let first = List.hd lst in
        let last = List.(nth lst (length lst - 1)) in
        ok (lst, environment_wrap (snd first).pre_environment (snd last).post_environment)

  let statements (lst:(environment -> statement) list) e : statement list =
    let rec aux lst e = match lst with
      | [] -> []
      | hd :: tl ->
          let s = hd e in
          s :: aux tl (snd s).post_environment
    in
    aux lst e

end

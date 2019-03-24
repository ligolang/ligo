open Ligo_helpers
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
  | Unit
  | Bool
  | Int | Nat | Float
  | String | Bytes

type type_value = [
  | `Pair of type_value * type_value
  | `Or of type_value * type_value
  | `Function of type_value * type_value
  | `Deep_closure of environment_small * type_value * type_value
  | `Shallow_closure of environment * type_value * type_value
  | `Base of type_base
]

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

type value = [
  | `Unit
  | `Bool of bool
  | `Nat of int
  | `Int of int
  | `String of string
  | `Bytes of bytes
  | `Pair of value * value
  | `Left of value
  | `Right of value
  (* | `Macro of anon_macro ... The future. *)
  | `Function of anon_function
]

and expression' =
  | Literal of value
  | Function_expression of anon_function_expression
  | Predicate of string * expression list
  | Apply of expression * expression
  | Var of var_name

and expression = expression' * type_value * environment (* Environment in which the expressions are evaluated *)

and assignment = var_name * expression

and statement' =
  | Assignment of assignment
  | Cond of expression * block * block
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

let expression_to_value ((e, _, _):expression) : value result =
  match e with
  | Literal v -> ok v
  | _ -> simple_fail "not a value"

module PP = struct
  open Format

  let space_sep ppf () = fprintf ppf " "

  let type_base ppf : type_base -> _ = function
    | Unit -> fprintf ppf "unit"
    | Bool -> fprintf ppf "bool"
    | Int -> fprintf ppf "int"
    | Float -> fprintf ppf "float"
    | Nat -> fprintf ppf "nat"
    | String -> fprintf ppf "string"
    | Bytes -> fprintf ppf "bytes"

  let rec type_ ppf : type_value -> _ = function
    | `Or(a, b) -> fprintf ppf "(%a) | (%a)" type_ a type_ b
    | `Pair(a, b) -> fprintf ppf "(%a) & (%a)" type_ a type_ b
    | `Base b -> type_base ppf b
    | `Function(a, b) -> fprintf ppf "(%a) -> (%a)" type_ a type_ b
    | `Shallow_closure(_, a, b) -> fprintf ppf "[big_closure](%a) -> (%a)" type_ a type_ b
    | `Deep_closure(c, arg, ret) ->
      fprintf ppf "[%a](%a)->(%a)"
        environment_small c
        type_ arg type_ ret

  and environment_element ppf ((s, tv) : environment_element) =
    Format.fprintf ppf "%s : %a" s type_ tv

  and environment_small' ppf = let open Append_tree in function
    | Leaf x -> environment_element ppf x
    | Node {a; b ; full ; size} ->
      fprintf ppf "@[<v 2>N(f:%b,s:%d)[@;%a,@;%a@]@;]"
        full size
        environment_small' a environment_small' b

  and environment_small ppf = function
    | Empty -> fprintf ppf "[]"
    | Full x -> environment_small' ppf x

  and environment_small_hlist' ppf = let open Append_tree in function
    | Leaf x -> environment_element ppf x
    | Node {a;b} ->
      fprintf ppf "%a, %a"
        environment_small_hlist' a
        environment_small_hlist' b

  and environment_small_hlist ppf = let open Append_tree in function
    | Empty -> fprintf ppf ""
    | Full x -> environment_small_hlist' ppf x

  let environment ppf (x:environment) = Format.pp_print_list environment_small ppf x

  let rec value ppf : value -> _ = function
    | `Bool b -> fprintf ppf "%b" b
    | `Int n -> fprintf ppf "%d" n
    | `Nat n -> fprintf ppf "%d" n
    | `Unit -> fprintf ppf " "
    | `String s -> fprintf ppf "\"%s\"" s
    | `Bytes _ -> fprintf ppf "[bytes]"
    | `Pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
    | `Left a -> fprintf ppf "L(%a)" value a
    | `Right b -> fprintf ppf "R(%a)" value b
    | `Function x -> function_ ppf x.content

  and expression ppf ((e, _, _):expression) = match e with
    | Var v -> fprintf ppf "%s" v
    | Apply(a, b) -> fprintf ppf "(%a)@(%a)" expression a expression b
    | Predicate(p, lst) -> fprintf ppf "%s %a" p (pp_print_list ~pp_sep:space_sep expression) lst
    | Literal v -> fprintf ppf "%a" value v
    | Function_expression c -> function_ ppf c

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
    | Cond (expr, i, e) -> fprintf ppf "if (%a) %a %a" expression expr block i block e
    | While (e, b) -> fprintf ppf "while (%a) %a" expression e block b

  and block ppf ((b, _):block) =
    fprintf ppf "@[<v 2>{@,%a@]@,}" (pp_print_list ~pp_sep:pp_print_newline statement) b

  let tl_statement ppf (ass, _) = assignment ppf ass

  let program ppf (p:program) =
    fprintf ppf "Program:\n---\n%a" (pp_print_list ~pp_sep:pp_print_newline tl_statement) p
end

module Free_variables = struct
  type free_variable = string
  type free_variables = free_variable list
  type t' = free_variable
  type t = free_variables

  let append_wd (* without doubles *) double x t =
    if List.mem x double
    then t
    else x :: t

  let append_bound x t = append_wd t x t

  let rec expression' (bound:t) : expression' -> t = function
    | Literal _ -> []
    | Var x -> append_wd bound x []
    | Predicate(_, exprs) -> List.(concat @@ map (expression bound) exprs)
    | Apply(a, b) -> List.(concat @@ map (expression bound) [a;b])
    | Function_expression {binder;body;result} -> block (binder :: bound) body @ expression (binder :: bound) result

  and expression bound (expr, _, _) = expression' bound expr

  and statement bound ((s, _) : statement) : (t * t) = match s with
    | Assignment (n, e) -> append_bound n bound, expression bound e
    | Cond (e, a, b) -> bound, (expression bound e) @ (block bound a) @ (block bound b)
    | While (e, b) -> bound, (expression bound e) @ (block bound b)

  and block' bound (b:block') : t = match b with
    | [] -> []
    | hd :: tl ->
      let (bound, fv) = statement bound hd in
      let fv' = block' bound tl in
      fv @ fv'

  and block bound (b, _ : block) : t = block' bound b

  let function_ ({content = {body ; binder ; result}} : anon_function) : t =
    block [binder] body @ expression [binder] result
end

module Translate_type = struct
  open Tezos_utils.Micheline.Michelson

  module Ty = struct

    let base_type : type_base -> ex_ty result =
      function
      | Unit -> ok @@ Ex_ty Types.unit
      | Bool -> ok @@ Ex_ty Types.bool
      | Int -> ok @@ Ex_ty Types.int
      | _ -> simple_fail "all based types not supported yet"

    let rec type_ : type_value -> ex_ty result =
      function
      | `Base b -> base_type b
      | `Pair (t, t') -> (
          type_ t >>? fun (Ex_ty t) ->
          type_ t' >>? fun (Ex_ty t') ->
          ok @@ Ex_ty (Types.pair t t')
        )
      | `Or (t, t') -> (
          type_ t >>? fun (Ex_ty t) ->
          type_ t' >>? fun (Ex_ty t') ->
          ok @@ Ex_ty (Types.union t t')
        )
      | `Function (arg, ret) ->
          let%bind (Ex_ty arg) = type_ arg in
          let%bind (Ex_ty ret) = type_ ret in
          ok @@ Ex_ty (Types.lambda arg ret)
      | `Deep_closure (c, arg, ret) ->
          let%bind (Ex_ty capture) = environment_small c in
          let%bind (Ex_ty arg) = type_ arg in
          let%bind (Ex_ty ret) = type_ ret in
          ok @@ Ex_ty Types.(pair capture @@ lambda (pair capture arg) ret)
      | `Shallow_closure (c, arg, ret) ->
          let%bind (Ex_ty capture) = environment c in
          let%bind (Ex_ty arg) = type_ arg in
          let%bind (Ex_ty ret) = type_ ret in
          ok @@ Ex_ty Types.(pair capture @@ lambda (pair capture arg) ret)


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


  let base_type : type_base -> michelson result =
    function
    | Unit -> ok @@ prim T_unit
    | Bool -> ok @@ prim T_bool
    | Int -> ok @@ prim T_int
    | _ -> simple_fail "all based types not supported yet"

  let rec type_ : type_value -> michelson result =
    function
    | `Base b -> base_type b
    | `Pair (t, t') -> (
        type_ t >>? fun t ->
        type_ t' >>? fun t' ->
        ok @@ prim ~children:[t;t'] T_pair
      )
    | `Or (t, t') -> (
        type_ t >>? fun t ->
        type_ t' >>? fun t' ->
        ok @@ prim ~children:[t;t'] T_or
      )
    | `Function (arg, ret) ->
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ prim ~children:[arg;ret] T_lambda
    | `Deep_closure (c, arg, ret) ->
      let%bind capture = environment_small c in
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ t_pair capture (t_lambda (t_pair capture arg) ret)
    | `Shallow_closure (c, arg, ret) ->
      let%bind capture = environment c in
      let%bind arg = type_ arg in
      let%bind ret = type_ ret in
      ok @@ t_pair capture (t_lambda (t_pair capture arg) ret)

  and environment_element (name, tyv) =
    let%bind michelson_type = type_ tyv in
    ok @@ annotate ("@" ^ name) michelson_type

  and environment_small' = let open Append_tree in function
    | Leaf x -> environment_element x
    | Node {a;b} ->
      let%bind a = environment_small' a in
      let%bind b = environment_small' b in
      ok @@ t_pair a b

  and environment_small = function
    | Empty -> ok @@ prim T_unit
    | Full x -> environment_small' x

  and environment =
    function
    | [] -> simple_fail "Schema.Big.to_michelson_type"
    | [a] -> environment_small a
    | a :: b ->
      let%bind a = environment_small a in
      let%bind b = environment b in
      ok @@ t_pair a b

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
      | Leaf (n, tv) -> ok (Var n, tv, env)
      | Node {a;b} ->
        let%bind ((_, ty_a, _) as a) = to_mini_c_capture' env a in
        let%bind ((_, ty_b, _) as b) = to_mini_c_capture' env b in
        ok (Predicate ("PAIR", [a;b]), `Pair(ty_a, ty_b), env)

    let to_mini_c_capture env = function
      | Empty -> simple_fail "to_mini_c_capture"
      | Full x -> to_mini_c_capture' env x

    let rec to_mini_c_type' = function
      | Leaf (_, t) -> t
      | Node {a;b} -> `Pair(to_mini_c_type' a, to_mini_c_type' b)

    let to_mini_c_type = function
      | Empty -> `Base Unit
      | Full x -> to_mini_c_type' x
  end

  type t = environment

  let empty : t = [Small.empty]
  let extend t : t = Small.empty :: t
  let restrict t : t = List.tl t
  let of_small small : t = [small]

  let has x : t -> bool = function
    | [] -> raise (Failure "Schema.Big.has")
    | hd :: _ -> Small.has x hd
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
    | hd :: tl -> `Pair(Small.to_mini_c_type hd, to_mini_c_type tl)
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
          | Errors _ -> aux b str
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
          "\ncode : %a\nschema type : %a"
          Tezos_utils.Micheline.Michelson.pp code
          Tezos_utils.Micheline.Michelson.pp schema_michelson
      in
      let%bind _ =
        Trace.trace_tzresult_lwt (error "error parsing big.get code" error_message) @@
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

  let simple_unary c = Unary ( seq [
      i_unpair ; c ; i_pair ;
    ])

  let simple_binary c = Binary ( seq [
      i_unpair ; dip i_unpair ; c ; i_pair ;
    ])

  let rec get_predicate : string -> predicate result = function
    | "ADD_INT" -> ok @@ simple_binary @@ prim I_ADD
    | "NEG" -> ok @@ simple_unary @@ prim I_NEG
    | "PAIR" -> ok @@ simple_binary @@ prim I_PAIR
    | x -> simple_fail @@ "predicate \"" ^ x ^ "\" doesn't exist"

  and translate_value (v:value) : michelson result = match v with
    | `Bool b -> ok @@ prim (if b then D_True else D_False)
    | `Int n -> ok @@ int (Z.of_int n)
    | `Nat n -> ok @@ int (Z.of_int n)
    | `String s -> ok @@ string s
    | `Bytes s -> ok @@ bytes (Tezos_stdlib.MBytes.of_bytes s)
    | `Unit -> ok @@ prim D_Unit
    | `Pair (a, b) -> (
        let%bind a = translate_value a in
        let%bind b = translate_value b in
        ok @@ prim ~children:[a;b] D_Pair
      )
    | `Left a -> translate_value a >>? fun a -> ok @@ prim ~children:[a] D_Left
    | `Right b -> translate_value b >>? fun b -> ok @@ prim ~children:[b] D_Right
    | `Function anon -> translate_function anon

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
      | Literal v ->
        let%bind v = translate_value v in
        let%bind t = Translate_type.type_ ty in
        ok @@ seq [
          prim ~children:[t;v] I_PUSH ;
          prim I_PAIR ;
        ]
      | Apply((_, f_ty, _) as f, arg) -> (
          match f_ty with
          | `Function _ -> (
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
          | `Deep_closure (small_env, _, _) -> (
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
          | `Shallow_closure (env', _, _) -> (
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
          | _ -> simple_fail "Applying something not appliable"
        )
      | Var x ->
        let%bind (get, _) = Environment.to_michelson_get env x in
        ok @@ seq [
          dip (seq [prim I_DUP ; get]) ;
          i_piar ;
        ]
      | Predicate(str, lst) ->
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
      | Function_expression anon -> (
          match ty with
          | `Function (_, _) ->
              let%bind body = translate_function_body anon in
              let%bind input_type = Translate_type.type_ anon.input in
              let%bind output_type = Translate_type.type_ anon.output in
              let code = seq [
                  i_lambda input_type output_type body ;
                  i_pair ;
                ] in
              ok code
          | `Deep_closure (small_env, _, _) ->
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
          | `Shallow_closure (_, _, _) ->
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
        ) in

    let%bind () =
      let%bind (Ex_ty schema_ty) = Environment.to_ty env in
      let%bind output_ty = Translate_type.type_ ty in
      let%bind (Ex_ty output_ty) =
        let error_message = Format.asprintf "%a" Michelson.pp output_ty in
        Trace.trace_tzresult_lwt (error "error parsing output ty" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson_ty output_ty in
      let input_stack_ty = Stack.(Types.unit @: schema_ty @: nil) in
      let output_stack_ty = Stack.(Types.(pair output_ty unit) @: schema_ty @: nil) in
      let%bind error_message =
        let%bind schema_michelson = Environment.to_michelson_type env in
        ok @@ Format.asprintf
          "expression : %a\ncode : %a\nschema type : %a"
          PP.expression (expr', ty, env)
          Tezos_utils.Micheline.Michelson.pp code
          Tezos_utils.Micheline.Michelson.pp schema_michelson
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
    | Cond (expr, a, b) ->
      let%bind expr = translate_expression expr in
      let%bind a = translate_regular_block a in
      let%bind b = translate_regular_block b in
      ok @@ (seq [
        prim ~children:[prim T_unit ; prim D_Unit] I_PUSH ;
        expr ;
        prim I_CAR ;
        dip Environment.to_michelson_extend ;
        prim ~children:[seq [a ; Environment.to_michelson_restrict];seq [b ; Environment.to_michelson_restrict]] I_IF ;
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
      | name , (Function_expression f, `Function (_, _), _) when f.capture_type = No_capture && name = entry -> Some f
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
        ok @@ `Pair(a, b)
      )
    | Union_t ((a_ty, _), _, _), L a -> (
        let%bind a = translate_value @@ Ex_typed_value(a_ty, a) in
        ok @@ `Left a
      )
    | Union_t (_, (b_ty, _), _), R b -> (
        let%bind b = translate_value @@ Ex_typed_value(b_ty, b) in
        ok @@ `Right b
      )
    | (Int_t _), n ->
        let%bind n =
          trace_option (simple_error "too big to fit an int") @@
          Alpha_context.Script_int.to_int n in
        ok @@ `Int n
    | (Nat_t _), n ->
        let%bind n =
          trace_option (simple_error "too big to fit an int") @@
          Alpha_context.Script_int.to_int n in
        ok @@ `Nat n
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

end


module Combinators = struct

  let get_bool (v:value) = match v with
    | `Bool b -> ok b
    | _ -> simple_fail "not a bool"

  let get_int (v:value) = match v with
    | `Int n -> ok n
    | _ -> simple_fail "not an int"

  let get_string (v:value) = match v with
    | `String s -> ok s
    | _ -> simple_fail "not a string"

  let get_bytes (v:value) = match v with
    | `Bytes b -> ok b
    | _ -> simple_fail "not a bytes"

  let get_unit (v:value) = match v with
    | `Unit -> ok ()
    | _ -> simple_fail "not a bool"

  let get_pair (v:value) = match v with
    | `Pair (a, b) -> ok (a, b)
    | _ -> simple_fail "not a pair"

  let get_left (v:value) = match v with
    | `Left b -> ok b
    | _ -> simple_fail "not a left"

  let get_right (v:value) = match v with
    | `Right b -> ok b
    | _ -> simple_fail "not a right"

  let get_or (v:value) = match v with
    | `Left b -> ok (false, b)
    | `Right b -> ok (true, b)
    | _ -> simple_fail "not a left/right"

  let get_last_statement ((b', _):block) : statement result =
    let aux lst = match lst with
      | [] -> simple_fail "get_last: empty list"
      | lst -> ok List.(nth lst (length lst - 1)) in
    aux b'

  let t_int : type_value = `Base Int

  let quote binder input output body result : anon_function =
    let content : anon_function_content = {
      binder ; input ; output ;
      body ; result ; capture_type = No_capture ;
    } in
    { content ; capture = None }

  let basic_quote i o b : anon_function result =
    let%bind (_, e) = get_last_statement b in
    let r : expression = (Var "output", o, e.post_environment) in
    ok @@ quote "input" i o b r

  let basic_int_quote b : anon_function result =
    basic_quote t_int t_int b

  let basic_int_quote_env : environment =
    let e = Environment.empty in
    Environment.add ("input", t_int) e

  let expr_int expr env : expression = (expr, t_int, env)
  let var_int name env : expression = expr_int (Var name) env

  let environment_wrap pre_environment post_environment = { pre_environment ; post_environment }
  let id_environment_wrap e = environment_wrap e e

  let statement s' e : statement =
    match s' with
    | Cond _ -> s', id_environment_wrap e
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

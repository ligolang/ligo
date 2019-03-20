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
  | `Closure of environment_small * type_value * type_value
  | `Base of type_base
]

and environment_element = string * type_value

and environment_small' = environment_element Append_tree.t'

and environment_small = environment_element Append_tree.t

and environment = environment_small list

type var_name = string
type fun_name = string

let get_new_name =
  let id = ref 0 in
  fun str -> (
      id := !id + 1;
      "_" ^ str ^ "_" ^ (string_of_int !id)
    )

type value = [
  | `Unit
  | `Bool of bool
  | `Nat of int
  | `Int of int
  | `String of string
  | `Pair of value * value
  | `Left of value
  | `Right of value
  | `Function of anon_function (* Actually a macro *)
  | `Closure of anon_closure
]

and expression' =
  | Literal of value
  | Predicate of string * expression list
  | Apply of expression * expression
  | Var of var_name

and expression = expression' * type_value

and assignment =
  | Fun of fun_name * anon_function
  | Variable of var_name * expression

and statement =
  | Assignment of assignment
  | Cond of expression * block * block
  | While of expression * block

and anon_function = {
  input : type_value ;
  output : type_value ;
  body : block ;
}

and anon_closure = {
  capture : value ;
  anon_function : anon_function ;
}

and block = statement list

and toplevel_statement = assignment

and program = toplevel_statement list

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
    | `Closure(c, arg, ret) ->
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
    | `Pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
    | `Left a -> fprintf ppf "L(%a)" value a
    | `Right b -> fprintf ppf "R(%a)" value b
    | `Function x -> function_ ppf x
    | `Closure {capture;anon_function} ->
      fprintf ppf "[%a]%a"
        value capture
        function_ anon_function

  and expression ppf ((e, _):expression) = match e with
    | Var v -> fprintf ppf "%s" v
    | Apply(a, b) -> fprintf ppf "(%a)@(%a)" expression a expression b
    | Predicate(p, lst) -> fprintf ppf "%s %a" p (pp_print_list ~pp_sep:space_sep expression) lst
    | Literal v -> fprintf ppf "%a" value v

  and function_ ppf ({input ; output ; body}:anon_function) =
        fprintf ppf "fun (%a) : %a %a"
          type_ input
          type_ output
          block body

  and assignment ppf (ass:assignment) =
    match ass with
    | Variable (n, e) -> fprintf ppf "let %s = %a;" n expression e
    | Fun (n, f) -> fprintf ppf "let %s = %a" n function_ f

  and statement ppf : statement -> _ = function
    | Assignment ass -> assignment ppf ass
    | Cond (expr, i, e) -> fprintf ppf "if (%a) %a %a" expression expr block i block e
    | While (e, b) -> fprintf ppf "while (%a) %a" expression e block b

  and block ppf (block:block) =
    fprintf ppf "@[<v 2>{@,%a@]@,}" (pp_print_list ~pp_sep:pp_print_newline statement) block

  let tl_statement = assignment

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

  and expression bound expr = expression' bound (fst expr)

  let rec statement bound : statement -> (t * t) = function
    | Assignment (Variable (n, e)) -> append_bound n bound, expression bound e
    | Assignment (Fun (n, f)) -> append_bound n bound, block (append_bound "input" @@ append_bound "output" bound) f.body
    | Cond (e, a, b) -> bound, (expression bound e) @ (block bound a) @ (block bound b)
    | While (e, b) -> bound, (expression bound e) @ (block bound b)

  and block bound : block -> t = function
    | [] -> []
    | hd :: tl ->
      let (bound, fv) = statement bound hd in
      let fv' = block bound tl in
      fv @ fv'

  let function_ ({body} : anon_function) : t =
    block ["input" ; "output"] body
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
      | `Closure (c, arg, ret) ->
        let%bind (Ex_ty capture) = environment_small c in
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

    let rec environment = function
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
    | `Closure (c, arg, ret) ->
      let%bind capture = environment_small c in
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

  let rec environment =
    function
    | [] -> simple_fail "Schema.Big.to_michelson_type"
    | [a] -> environment_small a
    | a :: b ->
      let%bind a = environment_small a in
      let%bind b = environment b in
      ok @@ t_pair a b

end

module Math = struct

  let lt_power_of_two n =
    let rec aux prev n =
      let cur = prev * 2 in
      if cur >= n
      then prev
      else aux cur n
    in
    if n > 0
    then ok (aux 1 n)
    else fail @@ error "lt_power_of_two" (string_of_int n)

  let ge_power_of_two n =
    let rec aux c n =
      if c >= n
      then c
      else aux (c * 2) n
    in
    if n > 0
    then ok (aux 1 n)
    else fail @@ error "ge_power_of_two" (string_of_int n)

  let rec exp x n =
    if n = 0
    then 1
    else
      let exp' = exp (x * x) (n / 2) in
      let m = if n mod 2 = 0 then 1 else x in
      m * exp'

  let exp2 = exp 2

  let log2_c n =
    let rec aux acc n =
      if n = 1
      then acc
      else aux (acc + 1) (n / 2)
    in
    if n < 1 then raise @@ Failure ("log_2") ;
    let n' = aux 0 n in
    if exp2 n' = n then n' else n' + 1

  let int_to_bools n l =
    let rec aux acc n = function
      | 0 -> acc
      | s -> aux ((n mod 2 = 0) :: acc) (n / 2) (s - 1)
    in
    List.rev @@ aux [] n l

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

    let rec env_of_expression bound prev ((e, tv) : expression) : t = match e with
      | Var n -> if List.mem n bound then prev else append (n, tv) prev
      | Literal _ -> prev
      | Apply (a, b) ->
        let prev = env_of_expression bound prev a in
        let prev = env_of_expression bound prev b in
        prev
      | Predicate (_, exprs) ->
        List.fold_left (env_of_expression bound) prev exprs

    let rec env_of_statement bound prev : statement -> (bound * t) = function
      | Assignment (Variable (n, expr)) ->
        let bound = n :: bound in
        bound, env_of_expression bound prev expr
      | Assignment (Fun (n, {body})) ->
        let bound = n :: bound in
        bound, env_of_block bound prev body
      | Cond (expr, ba, bb) ->
        let prev = env_of_expression bound prev expr in
        let prev = env_of_block bound prev ba in
        let prev = env_of_block bound prev bb in
        (bound, prev)
      | While (expr, b) ->
        let prev = env_of_expression bound prev expr in
        let prev = env_of_block bound prev b in
        (bound, prev)

    and env_of_block (bound:string list) prev : block -> t = function
      | [] -> prev
      | hd :: tl ->
        let (bound, prev) = env_of_statement bound prev hd in
        env_of_block bound prev tl

    let env_of_anon ({body} : anon_function) : t =
      let init = empty in
      env_of_block ["input"] init body

    let init_function input : t =
      append ("input", input) @@
      empty

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

    let rec to_mini_c_capture' = function
      | Leaf (n, tv) -> ok (Var n, tv)
      | Node {a;b} ->
        let%bind ((_, ty_a) as a) = to_mini_c_capture' a in
        let%bind ((_, ty_b) as b) = to_mini_c_capture' b in
        ok (Predicate ("PAIR", [a;b]), `Pair(ty_a, ty_b))

    let to_mini_c_capture = function
      | Empty -> simple_fail "sub env fail"
      | Full x -> to_mini_c_capture' x

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

  let init_function f : t = [Small.init_function f]

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

  and translate_value s (v:value) : michelson result = match v with
    | `Bool b -> ok @@ prim (if b then D_True else D_False)
    | `Int n -> ok @@ int (Z.of_int n)
    | `Nat n -> ok @@ int (Z.of_int n)
    | `String s -> ok @@ string s
    | `Unit -> ok @@ prim D_Unit
    | `Pair (a, b) -> (
        let%bind a = translate_value s a in
        let%bind b = translate_value s b in
        ok @@ prim ~children:[a;b] D_Pair
      )
    | `Left a -> translate_value s a >>? fun a -> ok @@ prim ~children:[a] D_Left
    | `Right b -> translate_value s b >>? fun b -> ok @@ prim ~children:[b] D_Right
    | `Function _ -> simple_fail "translating value : function"
    | `Closure _ -> simple_fail "translating value : closure"

  and translate_expression (s:Environment.t) ((e, ty):expression) : michelson result =
    let error_message = Format.asprintf  "%a" PP.expression (e, ty) in
    let%bind (code : michelson) = trace (error "translating expression" error_message) @@ match e with
      | Literal v ->
        let%bind v = translate_value s v in
        let%bind t = Translate_type.type_ ty in
        ok @@ seq [
          prim ~children:[t;v] I_PUSH ;
          prim I_PAIR ;
        ]
      | Apply(f, arg) -> (
          match snd f with
          | `Function _ -> (
              let%bind f = translate_expression s f in
              let%bind arg = translate_expression s arg in
              ok @@ seq [
                arg ;
                i_unpair ;
                dip f ;
                dip i_unpair ;
                prim I_EXEC ;
                i_pair ;
              ]
            )
          | `Closure _ -> (
              let%bind f = translate_expression s f in
              let%bind arg = translate_expression s arg in
              ok @@ seq [
                arg ;
                i_unpair ;
                dip f ;
                dip i_unpair ;
                dip i_unpair ;
                i_piar ;
                prim I_EXEC ;
                i_pair ;
              ]
            )
          | _ -> simple_fail "Applying something not appliable"
        )
      | Var x ->
        let%bind (get, _) = Environment.to_michelson_get s x in
        ok @@ seq [
          dip (seq [prim I_DUP ; get]) ;
          i_piar ;
        ]
      | Predicate(str, lst) ->
        let%bind lst = bind_list @@ List.map (translate_expression s) lst in
        let%bind predicate = get_predicate str in
        let%bind code = match (predicate, List.length lst) with
          | Constant c, 0 -> ok (seq @@ lst @ [c])
          | Unary f, 1 -> ok (seq @@ lst @ [f])
          | Binary f, 2 -> ok (seq @@ lst @ [f])
          | Ternary f, 3 -> ok (seq @@ lst @ [f])
          | _ -> simple_fail "bad arity"
        in
        ok code
    in

    let%bind () =
      let%bind (Ex_ty schema_ty) = Environment.to_ty s in
      let%bind output_ty = Translate_type.type_ ty in
      let%bind (Ex_ty output_ty) =
        let error_message = Format.asprintf "%a" Michelson.pp output_ty in
        Trace.trace_tzresult_lwt (error "error parsing output ty" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson_ty output_ty in
      let input_stack_ty = Stack.(Types.unit @: schema_ty @: nil) in
      let output_stack_ty = Stack.(Types.(pair output_ty unit) @: schema_ty @: nil) in
      let%bind error_message =
        let%bind schema_michelson = Environment.to_michelson_type s in
        ok @@ Format.asprintf
          "expression : %a\ncode : %a\nschema type : %a"
          PP.expression (e, ty)
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

  and translate_statement schema (s:statement) : (michelson * Environment.t) result =
    let error_message = Format.asprintf "%a" PP.statement s in
    let%bind ((code, new_schema) : michelson * Environment.t) =
      trace (error "translating statement" error_message) @@ match s with
    | Assignment (Variable (s, ((_, tv) as expr))) ->
      let%bind expr = translate_expression schema expr in
      let new_schema = Environment.add (s, tv) schema in
      let%bind add =
        if Environment.has s schema
        then Environment.to_michelson_set s schema
        else Environment.to_michelson_add (s, tv) schema
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
        ], new_schema)
    | Assignment (Fun (s, anon)) -> (
        match Environment.Small.env_of_anon anon with
        | Empty -> ( (* If there is no free variable, translate as a quote *)
            let env = Environment.init_function anon.input in
            let%bind body = translate_function_body env anon in
            let%bind input = Translate_type.type_ anon.input in
            let%bind output = Translate_type.type_ anon.output in
            let tv = `Function(anon.input, anon.output) in
            let new_schema = Environment.add (s, tv) schema in
            let%bind set = Environment.to_michelson_add (s, tv) schema in
            ok @@ (seq [
                i_lambda input output body ;
                set ;
              ], new_schema)
          )
        | (Full _) as small_env -> ( (* If there are free variables, translate as a closure *)
            let env = Environment.(of_small @@ Small.append ("input", anon.input) small_env) in
            let input = Environment.to_mini_c_type env in
            let%bind body = translate_function_body env ({anon with input}) in
            let%bind capture = Environment.Small.to_mini_c_capture small_env in
            let%bind capture = translate_expression schema capture in
            let tv : type_value = `Closure(small_env, anon.input, anon.output) in
            let%bind add = Environment.to_michelson_add (s, tv) schema in
            let%bind input_type = Translate_type.type_ input in
            let%bind output_type = Translate_type.type_ anon.output in
            let code = seq [
                i_push_unit ; capture ; i_car ;
                i_lambda input_type output_type body ;
                i_piar ;
                add ;
              ] in

            let new_schema = Environment.add (s, tv) schema in
            ok (code, new_schema)
          )
      )
    | Cond (expr, a, b) ->
      let new_schema = Environment.extend schema in
      let%bind expr = translate_expression schema expr in
      let%bind (a, _) = translate_regular_block new_schema a in
      let%bind (b, _) = translate_regular_block new_schema b in
      ok @@ (seq [
        prim ~children:[prim T_unit ; prim D_Unit] I_PUSH ;
        expr ;
        prim I_CAR ;
        dip Environment.to_michelson_extend ;
        prim ~children:[seq [a ; Environment.to_michelson_restrict];seq [b ; Environment.to_michelson_restrict]] I_IF ;
      ], schema)
    | While (expr, block) ->
      let%bind expr = translate_expression schema expr in
      let new_schema = Environment.extend schema in
      let%bind (block, _) = translate_regular_block new_schema block in
      ok @@ (seq [
        i_push_unit ; expr ; i_car ;
        dip Environment.to_michelson_extend ;
        prim ~children:[block ; Environment.to_michelson_restrict ; i_push_unit ; expr ; i_car] I_LOOP ;
      ], schema)
    in

    let%bind _assert_type =
      let%bind (Ex_ty schema_ty) = Environment.to_ty schema in
      let%bind (Ex_ty new_schema_ty) = Environment.to_ty new_schema in
      let%bind schema_michelson = Environment.to_michelson_type schema in
      let%bind new_schema_michelson = Environment.to_michelson_type new_schema in
      let input_stack_ty = Stack.(schema_ty @: nil) in
      let output_stack_ty = Stack.(new_schema_ty @: nil) in
      let%bind error_message =
        ok @@ Format.asprintf
          "\nstatement : %a\ncode : %a\nschema type : %a\nnew schema type : %a"
          PP.statement s
          Tezos_utils.Micheline.Michelson.pp code
          Tezos_utils.Micheline.Michelson.pp schema_michelson
          Tezos_utils.Micheline.Michelson.pp new_schema_michelson
      in
      let%bind _ =
        Trace.trace_tzresult_lwt (error "error parsing statement code" error_message) @@
        Tezos_utils.Memory_proto_alpha.parse_michelson code
          input_stack_ty output_stack_ty
      in
      ok ()
    in

    ok (code, new_schema)

  and translate_regular_block schema b : (michelson * Environment.t) result =
    let aux prev statement =
      let%bind ((lst, schema) : (michelson list * Environment.t)) = prev in
      let%bind (instruction, new_schema) = translate_statement schema statement in
      ok (instruction :: lst, new_schema)
    in
    let%bind error_message =
      let%bind schema_michelson = Environment.to_michelson_type schema in
      ok @@ Format.asprintf "\nblock : %a\nschema : %a\n"
        PP.block b
        Tezos_utils.Micheline.Michelson.pp schema_michelson
    in
    let%bind (codes, last_schema) =
      trace (error "error translating block" error_message) @@
      List.fold_left aux (ok ([], schema)) b in
    let code = seq (List.rev codes) in
    ok (code, last_schema)

  and translate_function_body env ({body} as f:anon_function) : michelson result =
    let schema = env in
    let%bind (body, post_schema) = translate_regular_block schema body in
    let%bind (get_output, _) = Environment.to_michelson_get post_schema "output" in
    let code = seq [
        body ;
        get_output ;
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

  let translate (p:program) : compiled_program result =
    let is_main = function
      | Fun ("main", f) -> Some f
      | _ -> None in
    let%bind main =
      trace_option (simple_error "no main") @@
      Tezos_utils.List.find_map is_main p in
    let {input;output} : anon_function = main in
    let%bind body = translate_function_body (Environment.init_function input) main in
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

module Translate_AST = struct

  module AST = Ligo_parser.Typed.O
  module SMap = Ligo_parser.Typed.SMap

  module Rename = struct
    open! AST

    let rec rename_expr_case (src:string) (dst:string) : expr_case -> expr_case = function
      | App {operator;arguments} -> App {operator = rename_operator src dst operator ; arguments = rename_exprs src dst arguments}
      | Var n when n.name.name = src -> Var {n with name = {n.name with name = dst}}
      | Var n -> Var n
      | Constant c -> Constant c
      | Record r -> Record (List.map (fun (key, expr) -> key, rename_expr src dst expr) r)
      | Lambda {parameter} as l when parameter.name.name = src -> l
      | Lambda ({instructions;declarations} as l) ->
         Lambda {l with instructions = rename_instrs src dst instructions ; declarations = rename_declarations src dst declarations}

    and rename_expr (src:string) (dst:string) (e : expr) : expr =
      { e with expr = rename_expr_case src dst e.expr }

    and rename_exprs src dst exprs = List.map (rename_expr src dst) exprs

    and rename_operator_case (src:string) (dst:string) : operator_case -> operator_case = function
      | Function n when n.name = src -> Function {n with name = dst}
      | x -> x

    and rename_operator src dst (o:operator) : operator = {o with operator = rename_operator_case src dst o.operator}

    and rename_var src dst (v:var_name) : var_name =
      if v.name = src
      then {v with name = dst}
      else v

    and rename_instr (src:string) (dst:string) : instr -> instr = function
      | Assignment {name;value;orig} when name.name = src -> Assignment {name = {name with name = dst};value;orig}
      | Assignment {name;value;orig} -> Assignment {value = rename_expr src dst value;name;orig}
      | While {condition;body;orig} -> While {condition = rename_expr src dst condition;body=rename_instrs src dst body;orig}
      | ForCollection {list;var;body;orig} -> ForCollection {list = rename_expr src dst list;var = rename_var src dst var;
                                                             body = rename_instrs src dst body;orig}
      | Match ({expr;cases} as a) -> Match {a with expr = rename_expr src dst expr ; cases = rename_match_cases src dst cases}
      | ProcedureCall {expr;orig} -> ProcedureCall {expr = rename_expr src dst expr;orig}
      | Fail {expr;orig} -> Fail {expr = rename_expr src dst expr;orig}

    and rename_instrs src dst : instr list -> instr list = List.map (rename_instr src dst)

    and rename_match_cases (src:string) (dst:string) (m:(_ * instr list) list) =
      List.map (fun (x, y) -> x, rename_instrs src dst y) m

    and rename_declaration (src:string) (dst:string) ({var} as d: decl) : decl =
      if var.name.name = src
      then {d with var = {var with name = {var.name with name = dst}}}
      else d

    and rename_declarations (src:string) (dst:string) (decls:decl list) =
      List.map (rename_declaration src dst) decls
  end

  let list_of_map m = List.rev @@ SMap.fold (fun _ v prev -> v :: prev) m []

  let rec translate_type : AST.type_expr -> type_value result = fun {type_expr}  ->
    match type_expr with
    | Unit -> ok (`Base Unit)
    | Int -> ok (`Base Int)
    | String -> ok (`Base String)
    | Bool -> ok (`Base Bool)
    | Sum m ->
       let node = Append_tree.of_list @@ List.map snd @@ list_of_map m in
       let aux a b : type_value result =
         let%bind a = a in
         let%bind b = b in
         ok (`Or (a, b))
       in
       Append_tree.fold_ne translate_type aux node
    | Record r ->
       let node = Append_tree.of_list @@ List.map snd @@ list_of_map r in
       let aux a b : type_value result =
         let%bind a = a in
         let%bind b = b in
         ok (`Pair (a, b))
       in
       Append_tree.fold_ne translate_type aux node
    | Ref t -> translate_type t
    | Function {arg;ret} ->
       let%bind arg = translate_type arg in
       let%bind ret = translate_type ret in
       ok (`Function(arg, ret))
    | TypeApp _ -> simple_fail "No type application"

  let translate_constant : AST.constant -> value result = function
    | Unit -> ok `Unit
    | String s -> ok (`String s)
    | Int n -> ok (`Int (Z.to_int n))
    | False -> ok (`Bool false)
    | True -> ok (`Bool true)
    | _ -> simple_fail ""

  let rec translate_lambda : AST.lambda -> anon_function result =
    fun {declarations;parameter;instructions;result} ->
       let ({name;ty}:AST.typed_var) = parameter in
       let%bind input_ty = translate_type ty in
       let declarations : AST.decl list = Rename.rename_declarations name.name "input" declarations in
       let instructions : AST.instr list = Rename.rename_instrs name.name "input" instructions in
       let%bind output_statement =
         let%bind (output_expr : expression) = translate_expr result in
         ok (Assignment (Variable("output", output_expr)))
       in
       let%bind output_ty = translate_type result.ty in
       let%bind (declaration_statements : statement list) = translate_declarations declarations in
       let%bind (instruction_statements : statement list) = translate_instructions instructions in
       let body = declaration_statements @ instruction_statements @ [output_statement] in
       ok {input=input_ty;output=output_ty;body}

  and translate_expr' : AST.expr_case -> expression' result = function
    | Var {name} -> ok (Var name.name)
    | Constant cst ->
       let%bind value = translate_constant cst in
       ok (Literal value)
    | Lambda _ -> simple_fail "Mini_c doesn't deal with lambda in expressions yet"
    | _ -> simple_fail ""

  and translate_expr : AST.expr -> expression result = fun {expr;ty} ->
    let%bind expr = translate_expr' expr in
    let%bind ty = translate_type ty in
    ok (expr, ty)

  and translate_declaration : AST.decl -> statement result = fun {var;value} ->
    let%bind expr = translate_expr value in
    ok (Assignment(Variable(var.name.name, expr)))

  and translate_declarations : AST.decl list -> statement list result = fun declarations ->
    bind_list @@ List.map translate_declaration declarations

  and translate_match (expr:AST.expr) (cases: (AST.pattern * AST.instr list) list) : statement result =
    match cases with
    | [(AST.PTrue, instrs_true) ; (AST.PFalse, instrs_false) ] ->
       let%bind cond = translate_expr expr in
       let%bind b_true = translate_instructions instrs_true in
       let%bind b_false = translate_instructions instrs_false in
       ok (Cond (cond, b_true, b_false))
    | [(AST.PFalse, instrs_false) ; (AST.PTrue, instrs_true) ] ->
       let%bind cond = translate_expr expr in
       let%bind b_true = translate_instructions instrs_true in
       let%bind b_false = translate_instructions instrs_false in
       ok (Cond (cond, b_true, b_false))
    | _ -> simple_fail "unrecognized pattern"

  and translate_instruction : AST.instr -> statement result = function
    | Assignment {name ; value} ->
       let%bind expr = translate_expr value in
       ok (Assignment (Variable(name.name, expr)))
    | While {condition ; body} ->
       let%bind block = translate_instructions body in
       let%bind cond = translate_expr condition in
       ok (While (cond, block))
    | ForCollection _ -> simple_fail "We don't deal with for collection yet"
    | Match {expr;cases} -> translate_match expr cases
    | Fail _ -> simple_fail "Fail have to be added in Mini_C"
    | ProcedureCall _ -> simple_fail "Drop Unit have to be added in Mini_C"

  and translate_instructions : AST.instr list -> statement list result = fun instrs ->
    bind_list @@ List.map translate_instruction instrs

  let translate_program : AST.ast -> block result = fun {declarations} ->
    translate_declarations declarations

  let rec to_mini_c_value' : (AST.expr_case * AST.type_expr) -> value result = function
    | Constant c, _ -> translate_constant c
    | App {arguments;operator = {operator = Constructor c ; ty = {type_expr = Sum lst}}}, _ ->
       let node = Append_tree.of_list @@ List.map fst @@ list_of_map lst in
       let%bind lst =
         trace_option (simple_error "Not constructor of variant type") @@
         Append_tree.exists_path (fun (x:AST.name_and_region) -> x.name = c.name) node in
       let arg = List.hd arguments in
       let%bind arg = to_mini_c_value arg in
       let ors = List.fold_left (fun b a -> if a then `Right b else `Left b) arg (List.rev lst) in
       ok ors
    | App _, _ -> simple_fail "Applications aren't value"
    | Record lst, _ ->
       let node = Append_tree.of_list @@ List.map snd lst in
       let aux a b =
         let%bind a = a in
         let%bind b = b in
         ok (`Pair (a, b))
       in
       Append_tree.fold_ne to_mini_c_value aux node
    | Lambda _, _-> simple_fail "Lambda aren't value yet"
    | Var _, _-> simple_fail "Var aren't value yet"

  and to_mini_c_value : AST.expr -> value result = fun {expr;ty} ->
    to_mini_c_value' (expr, ty)

  let ghost expr ty : AST.expr = {expr;ty;orig=`TODO}

  let of_mini_c_value ({type_expr} as ty, v : AST.type_expr * value) : AST.expr result = match (type_expr, v) with
    | String, `String s -> ok @@ ghost (Constant (String s)) ty
    | Bool, `Bool b -> ok @@ ghost (Constant (if b then True else False)) ty
    | Unit, `Unit -> ok @@ ghost (Constant (Unit)) ty
    | Int, `Int n -> ok @@ ghost (Constant (Int (Z.of_int n))) ty
    | Function _, _ -> simple_fail "Functions aren't retrieved from Mini_C yet"
    | _ -> simple_fail "of_mini_c_value error"
end

module Run = struct

  open Tezos_utils.Micheline

  let run_aux (program:program) (input_michelson:Michelson.t) : ex_typed_value result =
    let open Meta_michelson.Wrap in
    let%bind {input;output;body} = Translate_program.translate program in
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
    let%bind (Ex_typed_value (output_ty, output)) = run_aux program input in
    let%bind output =
      Trace.trace_tzresult_lwt (simple_error "error unparsing output") @@
      Tezos_utils.Memory_proto_alpha.unparse_michelson_data output_ty output in
    ok output

  let run (program:program) (input:value) : value result =
    let%bind input_michelson = Translate_program.translate_value Environment.empty input in
    let%bind ex_ty_value = run_aux program input_michelson in
    let%bind (result : value) = Translate_ir.translate_value ex_ty_value in
    ok result

end

module Combinators = struct

  let var x : expression' = Var x
  let apply a b : expression' = Apply(a, b)

  let t_int : type_value = `Base Int
  let type_int x : expression = x, `Base Int
  let type_f_int x : expression = x,`Function (`Base Int, `Base Int)
  let type_closure_int t x : expression = x, `Closure (t, `Base Int, `Base Int)
  let int n = type_int @@ Literal(`Int n)
  let neg_int x = type_int @@ Predicate("NEG", [x])
  let add_int x y = type_int @@ Predicate("ADD_INT", [x ; y])
  let var_int x = type_int @@ var x
  let apply_int a b = type_int @@ apply a b

  let assign_variable v expr = Assignment (Variable (v, expr))
  let assign_function v anon = Assignment (Fun (v, anon))
  let function_int body = {
    input = `Base Int ;
    output = `Base Int ;
    body ;
  }

end

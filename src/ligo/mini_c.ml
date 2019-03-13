open Ligo_helpers
open! Trace
open Tezos_utils.Memory_proto_alpha

open Script_typed_ir
open Script_ir_translator

module Michelson = Tezos_utils.Micheline.Michelson
module Stack = Meta_michelson.Wrap.Stack
module Types = Meta_michelson.Contract.Types

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

and environment_small' =
  | Leaf of environment_element
  | Node of {
      a : environment_small' ;
      b : environment_small' ;
      size : int ;
      full : bool ;
    }

and environment_small = Empty | Full of environment_small'

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

  and environment_small' ppf = function
    | Leaf x -> environment_element ppf x
    | Node {a; b ; full ; size} ->
      fprintf ppf "@[<v 2>N(f:%b,s:%d)[@;%a,@;%a@]@;]"
        full size
        environment_small' a environment_small' b

  and environment_small ppf = function
    | Empty -> fprintf ppf "[]"
    | Full x -> environment_small' ppf x

  and environment_small_hlist' ppf = function
    | Leaf x -> environment_element ppf x
    | Node {a;b} ->
      fprintf ppf "%a, %a"
        environment_small_hlist' a
        environment_small_hlist' b

  and environment_small_hlist ppf = function
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

    and environment_small' = function
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

  and environment_small' = function
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
    type t' = environment_small'
    type t = environment_small

    let node (a, b, size, full) = Node {a;b;size;full}

    let rec has' s = function
      | Leaf (s',_) when s = s' -> true
      | Leaf _ -> false
      | Node{a;b} -> has' s a || has' s b
    let has s = function
      | Empty -> false
      | Full x -> has' s x

    let empty : t = Empty

    let size' = function
      | Leaf _ -> 1
      | Node {size} -> size

    let size = function
      | Empty -> 0
      | Full x -> size' x

    let rec append' x = function
      | Leaf e -> node (Leaf e, Leaf x, 1, true)
      | Node({full=true;size}) as n -> node(n, Leaf x, size + 1, false)
      | Node({a=Node a;b;full=false} as n) -> (
          match append' x b with
          | Node{full=false} as b -> Node{n with b}
          | Node({full=true} as b) -> Node{n with b = Node b ; full = b.size = a.size}
          | Leaf _ -> assert false
        )
      | Node{a=Leaf _;full=false} -> assert false

    let append ((s, _) as x) = function
      | Empty -> Full (Leaf x)
      | Full t ->
        if has' s t then Full (t) else Full (append' x t)

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
        Trace.trace_tzresult_lwt (error "error parsing big.get code" error_message) @@
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

open Trace
open Types

module Expression = struct
  type t' = expression'
  type t = expression

  let get_content : t -> t' = fun e -> e.content
  let get_type : t -> type_value = fun e -> e.type_value
  let get_environment : t -> environment = fun e -> e.environment
  let is_toplevel : t -> bool = fun e -> e.is_toplevel

  let make = fun ?(itl = false) e' t env -> {
    content = e' ;
    type_value = t ;
    environment = env ;
    is_toplevel = itl ;
  }

  let make_tpl = fun ?(itl = false) (e' , t , env) -> {
    content = e' ;
    type_value = t ;
    environment = env ;
    is_toplevel = itl ;
  }

  let pair : t -> t -> t' = fun a b -> E_constant ("PAIR" , [ a ; b ])

end

let get_bool (v:value) = match v with
  | D_bool b -> ok b
  | _ -> simple_fail "not a bool"

let get_int (v:value) = match v with
  | D_int n -> ok n
  | _ -> simple_fail "not an int"

let get_nat (v:value) = match v with
  | D_nat n -> ok n
  | _ -> simple_fail "not a nat"

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

let get_list (v:value) = match v with
  | D_list lst -> ok lst
  | _ -> simple_fail "not a list"

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

let get_t_list (t:type_value) = match t with
  | T_list t -> ok t
  | _ -> simple_fail "not a type list"

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

let wrong_type name t =
  let title () = "not a " ^ name in
  let content () = Format.asprintf "%a" PP.type_ t in
  error title content

let get_t_left t = match t with
  | T_or (a , _) -> ok a
  | _ -> fail @@ wrong_type "union" t

let get_t_right t = match t with
  | T_or (_ , b) -> ok b
  | _ -> fail @@ wrong_type "union" t

let get_t_contract t = match t with
  | T_contract x -> ok x
  | _ -> fail @@ wrong_type "contract" t

let get_t_operation t = match t with
  | T_base Base_operation -> ok ()
  | _ -> fail @@ wrong_type "operation" t

let get_operation (v:value) = match v with
  | D_operation x -> ok x
  | _ -> simple_fail "not an operation"


let get_last_statement ((b', _):block) : statement result =
  let aux lst = match lst with
    | [] -> simple_fail "get_last: empty list"
    | lst -> ok List.(nth lst (length lst - 1)) in
  aux b'

let t_int : type_value = T_base Base_int
let t_nat : type_value = T_base Base_nat

let t_function x y : type_value = T_function ( x , y )
let t_deep_closure x y z : type_value = T_deep_closure ( x , y , z )
let t_pair x y : type_value = T_pair ( x , y )
let t_union x y : type_value = T_or ( x , y )

let quote binder input output body result : anon_function =
  {
    binder ; input ; output ;
    body ; result ;
  }

let basic_quote i o b : anon_function result =
  let%bind (_, e) = get_last_statement b in
  let r : expression = Expression.make_tpl (E_variable "output", o, e.post_environment) in
  ok @@ quote "input" i o b r

let basic_int_quote b : anon_function result =
  basic_quote t_int t_int b

let e_int expr env : expression = Expression.make_tpl (expr, t_int, env)
let e_var_int name env : expression = e_int (E_variable name) env

let d_unit : value = D_unit

let environment_wrap pre_environment post_environment = { pre_environment ; post_environment }
let id_environment_wrap e = environment_wrap e e

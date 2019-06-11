open Trace
open Types

module Expression = struct
  type t' = expression'
  type t = expression

  let get_content : t -> t' = fun e -> e.content
  let get_type : t -> type_value = fun e -> e.type_value
  let is_toplevel : t -> bool = fun e -> e.is_toplevel

  let make = fun ?(itl = false) e' t -> {
    content = e' ;
    type_value = t ;
    is_toplevel = itl ;
  }

  let make_tpl = fun ?(itl = false) (e' , t) -> {
    content = e' ;
    type_value = t ;
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

let get_timestamp (v:value) = match v with
  | D_timestamp n -> ok n
  | _ -> simple_fail "not a timestamp"

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

let get_set (v:value) = match v with
  | D_set lst -> ok lst
  | _ -> simple_fail "not a set"

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

let get_t_set (t:type_value) = match t with
  | T_set t -> ok t
  | _ -> simple_fail "not a type set"

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


let t_int : type_value = T_base Base_int
let t_unit : type_value = T_base Base_unit
let t_nat : type_value = T_base Base_nat

let t_function x y : type_value = T_function ( x , y )
let t_deep_closure x y z : type_value = T_deep_closure ( x , y , z )
let t_pair x y : type_value = T_pair ( x , y )
let t_union x y : type_value = T_or ( x , y )

let quote binder input output result : anon_function =
  {
    binder ; input ; output ;
    result ;
  }


let e_int expr : expression = Expression.make_tpl (expr, t_int)
let e_unit : expression = Expression.make_tpl (E_literal D_unit, t_unit)
let e_skip : expression = Expression.make_tpl (E_skip, t_unit)
let e_var_int name : expression = e_int (E_variable name)
let e_let_int v tv expr body : expression = Expression.(make_tpl (
    E_let_in ((v , tv) , expr , body) ,
    get_type body
  ))

let ez_e_sequence a b : expression = Expression.(make_tpl (E_sequence (make_tpl (a , t_unit) , b) , get_type b))

let ez_e_return e : expression = Expression.(make_tpl ((E_environment_return e) , get_type e))

let d_unit : value = D_unit

let basic_quote i o expr : anon_function result =
  ok @@ quote "input" i o (ez_e_return expr)

let basic_int_quote expr : anon_function result =
  basic_quote t_int t_int expr


let environment_wrap pre_environment post_environment = { pre_environment ; post_environment }
let id_environment_wrap e = environment_wrap e e

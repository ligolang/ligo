open Trace
open Types

module Expression = struct
  type t' = expression_content
  type t = expression

  let get_content : t -> t' = fun e -> e.content
  let get_type : t -> type_expression = fun e -> e.type_expression

  let make_t ?(loc=Location.generated) = fun tc -> {
    type_content = tc;
    location = loc;
  }

  let make ?(loc=Location.generated) = fun e' t -> {
    content = e' ;
    type_expression = t ;
    location = loc;
  }

  let make_tpl ?(loc=Location.generated) = fun (e' , t) -> {
    content = e' ;
    type_expression = t ;
    location = loc;
  }

  let pair : t -> t -> t' = fun a b -> E_constant { cons_name = C_PAIR; arguments = [ a ; b ]}

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

let get_mutez (v:value) = match v with
  | D_mutez n -> ok n
  | _ -> simple_fail "not a mutez"

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

let get_big_map (v:value) = match v with
  | D_big_map lst -> ok lst
  | _ -> simple_fail "not a big_map"

let get_list (v:value) = match v with
  | D_list lst -> ok lst
  | _ -> simple_fail "not a list"

let get_set (v:value) = match v with
  | D_set lst -> ok lst
  | _ -> simple_fail "not a set"

let get_function_with_ty (e : expression) =
  match (e.content , e.type_expression.type_content) with
  | E_closure f , T_function ty -> ok (f , ty)
  | _ -> simple_fail "not a function with functional type"

let get_function (e : expression) =
  match (e.content) with
  | E_closure f -> ok f
  | _ -> simple_fail "not a function"

let get_t_function tv = match tv.type_content with
  | T_function ty -> ok ty
  | _ -> simple_fail "not a function"

let get_t_option (v:type_expression) = match v.type_content with
  | T_option t -> ok t
  | _ -> simple_fail "not an option"

let get_pair (v:value) = match v with
  | D_pair (a, b) -> ok (a, b)
  | _ -> simple_fail "not a pair"

let get_t_pair (t:type_expression) = match t.type_content with
  | T_pair ((_, a), (_, b)) -> ok (a, b)
  | _ -> simple_fail "not a type pair"

let get_t_or (t:type_expression) = match t.type_content with
  | T_or ((_, a), (_, b)) -> ok (a, b)
  | _ -> simple_fail "not a type or"

let get_t_map (t:type_expression) = match t.type_content with
  | T_map kv -> ok kv
  | _ -> simple_fail "not a type map"

let get_t_big_map (t:type_expression) = match t.type_content with
  | T_big_map kv -> ok kv
  | _ -> simple_fail "not a type big_map"

let get_t_list (t:type_expression) = match t.type_content with
  | T_list t -> ok t
  | _ -> simple_fail "not a type list"

let get_t_set (t:type_expression) = match t.type_content with
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
  let content () = Format.asprintf "%a" PP.type_variable t in
  error title content

let get_t_left t = match t.type_content with
  | T_or ((_, a) , _) -> ok a
  | _ -> fail @@ wrong_type "union" t

let get_t_right t = match t.type_content with
  | T_or (_ , (_, b)) -> ok b
  | _ -> fail @@ wrong_type "union" t

let get_t_contract t = match t.type_content with
  | T_contract x -> ok x
  | _ -> fail @@ wrong_type "contract" t

let get_t_operation t = match t.type_content with
  | T_base TB_operation -> ok t
  | _ -> fail @@ wrong_type "operation" t

let get_operation (v:value) = match v with
  | D_operation x -> ok x
  | _ -> simple_fail "not an operation"


let t_int  ?loc () : type_expression = Expression.make_t ?loc @@ T_base TB_int
let t_unit ?loc () : type_expression = Expression.make_t ?loc @@ T_base TB_unit
let t_nat  ?loc () : type_expression = Expression.make_t ?loc @@ T_base TB_nat

let t_function ?loc x y : type_expression = Expression.make_t ?loc @@ T_function ( x , y )
let t_pair     ?loc x y : type_expression = Expression.make_t ?loc @@ T_pair ( x , y )
let t_union    ?loc x y : type_expression = Expression.make_t ?loc @@ T_or ( x , y )

let e_int  ?loc expr    : expression = Expression.make_tpl ?loc (expr, t_int ())
let e_unit ?loc ()      : expression = Expression.make_tpl ?loc (E_literal D_unit, t_unit ())
let e_skip ?loc ()      : expression = Expression.make_tpl ?loc (E_skip, t_unit ())
let e_var_int ?loc name : expression = e_int ?loc (E_variable name)
let e_let_in ?loc v tv inline expr body : expression = Expression.(make_tpl ?loc(
    E_let_in ((v , tv) , inline, expr , body) ,
    get_type body
  ))
let e_application ?loc f t arg: expression = Expression.(make_tpl ?loc(
    E_application (f,arg) ,
    t
  ))
let e_var ?loc vname t: expression = Expression.(make_tpl ?loc(
    E_variable vname ,
    t
  ))


let ez_e_sequence ?loc a b : expression = Expression.(make_tpl (E_sequence (make_tpl ?loc (a , t_unit ()) , b) , get_type b))

let d_unit : value = D_unit


let environment_wrap pre_environment post_environment = { pre_environment ; post_environment }
let id_environment_wrap e = environment_wrap e e

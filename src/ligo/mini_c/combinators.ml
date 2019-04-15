open Trace
open Types

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

let get_last_statement ((b', _):block) : statement result =
  let aux lst = match lst with
    | [] -> simple_fail "get_last: empty list"
    | lst -> ok List.(nth lst (length lst - 1)) in
  aux b'

let t_int : type_value = T_base Base_int
let t_nat : type_value = T_base Base_nat

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
  let e = Compiler_environment.empty in
  Compiler_environment.add ("input", t_int) e

let e_int expr env : expression = (expr, t_int, env)
let e_var_int name env : expression = e_int (E_variable name) env

let d_unit : value = D_unit

let environment_wrap pre_environment post_environment = { pre_environment ; post_environment }
let id_environment_wrap e = environment_wrap e e

let statement s' e : statement =
  match s' with
  | S_cond _ -> s', id_environment_wrap e
  | S_if_none _ -> s', id_environment_wrap e
  | S_while _ -> s', id_environment_wrap e
  | S_patch _ -> s', id_environment_wrap e
  | S_assignment (name, (_, t, _)) -> s', environment_wrap e (Compiler_environment.add (name, t) e)

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

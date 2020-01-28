open Trace
open Types

module Errors = struct
  let not_a_x_expression expected_expression actual_expression () =
    let message () =
      Format.asprintf "Expected a %s expression but got a %a expression"
        expected_expression
        PP.expression actual_expression in
    error (thunk "Expected a different expression") message

  let not_a_x_type expected_type actual_type () =
    let message () =
      Format.asprintf "Expected the type %s but got the type %a"
        expected_type
        PP.type_value actual_type in
    error (thunk "Expected a different type") message

  let declaration_not_found expected_declaration () =
    let message () =
      Format.asprintf "Could not find a declaration with the name %s"
        expected_declaration in
    error (thunk "No declaration with the given name") message
end

let make_t type_value' simplified = { type_value' ; simplified }
let make_a_e ?(location = Location.generated) expression type_annotation environment = {
  expression ;
  type_annotation ;
  environment ;
  location ;
}
let make_n_e name a_e = { name ; annotated_expression = a_e }
let make_n_t type_name type_value = { type_name ; type_value }

let t_signature ?s ()  : type_value = make_t (T_constant TC_signature) s
let t_chain_id ?s ()   : type_value = make_t (T_constant TC_chain_id) s
let t_bool ?s ()       : type_value = make_t (T_constant TC_bool) s
let t_string ?s ()     : type_value = make_t (T_constant TC_string) s
let t_bytes ?s ()      : type_value = make_t (T_constant TC_bytes) s
let t_key ?s ()        : type_value = make_t (T_constant TC_key) s
let t_key_hash ?s ()   : type_value = make_t (T_constant TC_key_hash) s
let t_int ?s ()        : type_value = make_t (T_constant TC_int) s
let t_address ?s ()    : type_value = make_t (T_constant TC_address) s
let t_operation ?s ()  : type_value = make_t (T_constant TC_operation) s
let t_nat ?s ()        : type_value = make_t (T_constant TC_nat) s
let t_mutez ?s ()      : type_value = make_t (T_constant TC_mutez) s
let t_timestamp ?s ()  : type_value = make_t (T_constant TC_timestamp) s
let t_unit ?s ()       : type_value = make_t (T_constant TC_unit) s
let t_option o ?s ()   : type_value = make_t (T_operator (TC_option o)) s
let t_tuple lst ?s ()  : type_value = make_t (T_operator (TC_tuple lst)) s
let t_variable t ?s () : type_value = make_t (T_variable t) s
let t_list t ?s ()     : type_value = make_t (T_operator (TC_list t)) s
let t_set t ?s ()      : type_value = make_t (T_operator (TC_set t)) s
let t_contract t ?s () : type_value = make_t (T_operator (TC_contract t)) s
let t_pair a b ?s ()   : type_value = t_tuple [a ; b] ?s ()

let t_record m ?s () : type_value = make_t (T_record m) s
let make_t_ez_record (lst:(label * type_value) list) : type_value =
  let aux prev (k, v) = LMap.add k v prev in
  let map = List.fold_left aux LMap.empty lst in
  make_t (T_record map) None
let ez_t_record lst ?s () : type_value =
  let m = LMap.of_list lst in
  t_record m ?s ()

let t_map key value ?s () = make_t (T_operator (TC_map (key , value))) s
let t_big_map key value ?s () = make_t (T_operator (TC_big_map (key , value))) s

let t_sum m ?s () : type_value = make_t (T_sum m) s
let make_t_ez_sum (lst:(constructor * type_value) list) : type_value =
  let aux prev (k, v) = CMap.add k v prev in
  let map = List.fold_left aux CMap.empty lst in
  make_t (T_sum map) None

let t_function param result ?s () : type_value = make_t (T_arrow (param, result)) s
let t_shallow_closure param result ?s () : type_value = make_t (T_arrow (param, result)) s

let get_type_annotation (x:annotated_expression) = x.type_annotation
let get_type' (x:type_value) = x.type_value'
let get_environment (x:annotated_expression) = x.environment
let get_expression (x:annotated_expression) = x.expression

let get_lambda e : _ result = match e with
  | E_lambda l -> ok l
  | _ -> fail @@ Errors.not_a_x_expression "lambda" e ()

let get_lambda_with_type e =
  match (e.expression , e.type_annotation.type_value') with
  | E_lambda l , T_arrow (i,o) -> ok (l , (i,o))
  | _ -> fail @@ Errors.not_a_x_expression "lambda with functional type" e.expression ()

let get_t_bool (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_bool) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "bool" t ()

let get_t_int (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_int) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "int" t ()

let get_t_nat (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_nat) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "nat" t ()

let get_t_unit (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_unit) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "unit" t ()

let get_t_mutez (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_mutez) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "tez" t ()

let get_t_bytes (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_bytes) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "bytes" t ()

let get_t_string (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_string) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "string" t ()

let get_t_contract (t:type_value) : type_value result = match t.type_value' with
  | T_operator (TC_contract x) -> ok x
  | _ -> fail @@ Errors.not_a_x_type "contract" t ()

let get_t_option (t:type_value) : type_value result = match t.type_value' with
  | T_operator (TC_option o) -> ok o
  | _ -> fail @@ Errors.not_a_x_type "option" t ()

let get_t_list (t:type_value) : type_value result = match t.type_value' with
  | T_operator (TC_list l) -> ok l
  | _ -> fail @@ Errors.not_a_x_type "list" t ()

let get_t_set (t:type_value) : type_value result = match t.type_value' with
  | T_operator (TC_set s) -> ok s
  | _ -> fail @@ Errors.not_a_x_type "set" t ()

let get_t_key (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_key) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "key" t ()

let get_t_signature (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_signature) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "signature" t ()

let get_t_key_hash (t:type_value) : unit result = match t.type_value' with
  | T_constant (TC_key_hash) -> ok ()
  | _ -> fail @@ Errors.not_a_x_type "key_hash" t ()

let get_t_tuple (t:type_value) : type_value list result = match t.type_value' with
  | T_operator (TC_tuple lst) -> ok lst
  | _ -> fail @@ Errors.not_a_x_type "tuple" t ()

let get_t_pair (t:type_value) : (type_value * type_value) result = match t.type_value' with
  | T_operator (TC_tuple lst) ->
      let%bind () =
        trace_strong (Errors.not_a_x_type "pair (tuple with two elements)" t ()) @@
        Assert.assert_list_size lst 2 in
      ok List.(nth lst 0 , nth lst 1)
  | _ -> fail @@ Errors.not_a_x_type "pair (tuple with two elements)" t ()

let get_t_function (t:type_value) : (type_value * type_value) result = match t.type_value' with
  | T_arrow (a,r) -> ok (a,r)
  | T_operator (TC_arrow (a , b)) -> ok (a , b)
  | _ -> fail @@ Errors.not_a_x_type "function" t ()

let get_t_sum (t:type_value) : type_value constructor_map result = match t.type_value' with
  | T_sum m -> ok m
  | _ -> fail @@ Errors.not_a_x_type "sum" t ()

let get_t_record (t:type_value) : type_value label_map result = match t.type_value' with
  | T_record m -> ok m
  | _ -> fail @@ Errors.not_a_x_type "record" t ()

let get_t_map (t:type_value) : (type_value * type_value) result =
  match t.type_value' with
  | T_operator (TC_map (k,v)) -> ok (k, v)
  | _ -> fail @@ Errors.not_a_x_type "map" t ()

let get_t_big_map (t:type_value) : (type_value * type_value) result =
  match t.type_value' with
  | T_operator (TC_big_map (k,v)) -> ok (k, v)
  | _ -> fail @@ Errors.not_a_x_type "big_map" t ()

let get_t_map_key : type_value -> type_value result = fun t ->
  let%bind (key , _) = get_t_map t in
  ok key

let get_t_map_value : type_value -> type_value result = fun t ->
  let%bind (_ , value) = get_t_map t in
  ok value

let get_t_big_map_key : type_value -> type_value result = fun t ->
  let%bind (key , _) = get_t_big_map t in
  ok key

let get_t_big_map_value : type_value -> type_value result = fun t ->
  let%bind (_ , value) = get_t_big_map t in
  ok value

let assert_t_map = fun t ->
  let%bind _ = get_t_map t in
  ok ()

let is_t_map = Function.compose to_bool get_t_map
let is_t_big_map = Function.compose to_bool get_t_big_map

let assert_t_mutez : type_value -> unit result = get_t_mutez
let assert_t_key = get_t_key
let assert_t_signature = get_t_signature
let assert_t_key_hash = get_t_key_hash

let assert_t_contract (t:type_value) : unit result = match t.type_value' with
  | T_operator (TC_contract _) -> ok ()
  | _ -> simple_fail "not a contract"

let assert_t_list t =
  let%bind _ = get_t_list t in
  ok ()

let is_t_list = Function.compose to_bool get_t_list
let is_t_set = Function.compose to_bool get_t_set
let is_t_nat = Function.compose to_bool get_t_nat
let is_t_string = Function.compose to_bool get_t_string
let is_t_bytes = Function.compose to_bool get_t_bytes
let is_t_int = Function.compose to_bool get_t_int

let assert_t_bytes = fun t ->
  let%bind _ = get_t_bytes t in
  ok ()

let assert_t_operation (t:type_value) : unit result =
  match t.type_value' with
  | T_constant (TC_operation) -> ok ()
  | _ -> simple_fail "assert: not an operation"

let assert_t_list_operation (t : type_value) : unit result =
  let%bind t' = get_t_list t in
  assert_t_operation t'

let assert_t_int : type_value -> unit result = fun t -> match t.type_value' with
  | T_constant (TC_int) -> ok ()
  | _ -> simple_fail "not an int"

let assert_t_nat : type_value -> unit result = fun t -> match t.type_value' with
  | T_constant (TC_nat) -> ok ()
  | _ -> simple_fail "not an nat"

let assert_t_bool : type_value -> unit result = fun v -> get_t_bool v
let assert_t_unit : type_value -> unit result = fun v -> get_t_unit v

let e_record map : expression = E_record map
let ez_e_record (lst : (label * ae) list) : expression =
  let aux prev (k, v) = LMap.add k v prev in
  let map = List.fold_left aux LMap.empty lst in
  e_record map
let e_some s : expression = E_constant (C_SOME, [s])
let e_none () : expression = E_constant (C_NONE, [])

let e_map lst : expression = E_map lst

let e_unit () : expression = E_literal (Literal_unit)
let e_int n : expression = E_literal (Literal_int n)
let e_nat n : expression = E_literal (Literal_nat n)
let e_mutez n : expression = E_literal (Literal_mutez n)
let e_bool b : expression = E_literal (Literal_bool b)
let e_string s : expression = E_literal (Literal_string s)
let e_bytes s : expression = E_literal (Literal_bytes s)
let e_timestamp s : expression = E_literal (Literal_timestamp s)
let e_address s : expression = E_literal (Literal_address s)
let e_signature s : expression = E_literal (Literal_signature s)
let e_key s : expression = E_literal (Literal_key s)
let e_key_hash s : expression = E_literal (Literal_key_hash s)
let e_chain_id s : expression = E_literal (Literal_chain_id s)
let e_operation s : expression = E_literal (Literal_operation s)
let e_lambda l : expression = E_lambda l
let e_pair a b : expression = E_tuple [a; b]
let e_application a b : expression = E_application (a , b)
let e_variable v : expression = E_variable v
let e_list lst : expression = E_list lst
let e_let_in binder inline rhs result = E_let_in { binder ; rhs ; result; inline }
let e_tuple lst : expression = E_tuple lst

let e_a_unit = make_a_e (e_unit ()) (t_unit ())
let e_a_int n = make_a_e (e_int n) (t_int ())
let e_a_nat n = make_a_e (e_nat n) (t_nat ())
let e_a_mutez n = make_a_e (e_mutez n) (t_mutez ())
let e_a_bool b = make_a_e (e_bool b) (t_bool ())
let e_a_string s = make_a_e (e_string s) (t_string ())
let e_a_address s = make_a_e (e_address s) (t_address ())
let e_a_pair a b = make_a_e (e_pair a b) (t_pair a.type_annotation b.type_annotation ())
let e_a_some s = make_a_e (e_some s) (t_option s.type_annotation ())
let e_a_lambda l in_ty out_ty = make_a_e (e_lambda l) (t_function in_ty out_ty ())
let e_a_none t = make_a_e (e_none ()) (t_option t ())
let e_a_tuple lst = make_a_e (E_tuple lst) (t_tuple (List.map get_type_annotation lst) ())
let e_a_record r = make_a_e (e_record r) (t_record (LMap.map get_type_annotation r) ())
let e_a_application a b = make_a_e (e_application a b) (get_type_annotation b)
let e_a_variable v ty = make_a_e (e_variable v) ty
let ez_e_a_record r = make_a_e (ez_e_record r) (ez_t_record (List.map (fun (x, y) -> x, y.type_annotation) r) ())
let e_a_map lst k v = make_a_e (e_map lst) (t_map k v ())
let e_a_list lst t = make_a_e (e_list lst) (t_list t ())
let e_a_let_in binder expr body attributes = make_a_e (e_let_in binder expr body attributes) (get_type_annotation body)

let get_a_int (t:annotated_expression) =
  match t.expression with
  | E_literal (Literal_int n) -> ok n
  | _ -> simple_fail "not an int"

let get_a_unit (t:annotated_expression) =
  match t.expression with
  | E_literal (Literal_unit) -> ok ()
  | _ -> simple_fail "not a unit"

let get_a_bool (t:annotated_expression) =
  match t.expression with
  | E_literal (Literal_bool b) -> ok b
  | _ -> simple_fail "not a bool"


let get_a_record_accessor = fun t ->
  match t.expression with
  | E_record_accessor (a , b) -> ok (a , b)
  | _ -> simple_fail "not an accessor"

let get_declaration_by_name : program -> string -> declaration result = fun p name ->
  let aux : declaration -> bool = fun declaration ->
    match declaration with
    | Declaration_constant (d , _, _) -> d.name = Var.of_name name
  in
  trace_option (Errors.declaration_not_found name ()) @@
  List.find_opt aux @@ List.map Location.unwrap p

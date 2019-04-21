open Trace
open Types

let make_t type_value' simplified = { type_value' ; simplified }
let make_a_e expression type_annotation environment = { expression ; type_annotation ; dummy_field = () ; environment }
let make_a_e_empty expression type_annotation = make_a_e expression type_annotation Environment.full_empty
let make_n_e name a_e = { name ; annotated_expression = a_e }

let t_bool ?s () : type_value = make_t (T_constant ("bool", [])) s
let t_string ?s () : type_value = make_t (T_constant ("string", [])) s
let t_bytes ?s () : type_value = make_t (T_constant ("bytes", [])) s
let t_int ?s () : type_value = make_t (T_constant ("int", [])) s
let t_nat ?s () : type_value = make_t (T_constant ("nat", [])) s
let t_unit ?s () : type_value = make_t (T_constant ("unit", [])) s
let t_option o ?s () : type_value = make_t (T_constant ("option", [o])) s
let t_tuple lst ?s () : type_value = make_t (T_tuple lst) s
let t_list t ?s () : type_value = make_t (T_constant ("list", [t])) s
let t_pair a b ?s () = t_tuple [a ; b] ?s ()

let t_record m ?s () : type_value = make_t (T_record m) s
let make_t_ez_record (lst:(string * type_value) list) : type_value =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  make_t (T_record map) None
let ez_t_record lst ?s () : type_value =
  let m = SMap.of_list lst in
  t_record m ?s ()

let t_map key value ?s () = make_t (T_constant ("map", [key ; value])) s

let t_sum m ?s () : type_value = make_t (T_sum m) s
let make_t_ez_sum (lst:(string * type_value) list) : type_value =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  make_t (T_sum map) None

let t_function param result ?s () : type_value = make_t (T_function (param, result)) s
let t_shallow_closure param result ?s () : type_value = make_t (T_function (param, result)) s

let get_type_annotation (x:annotated_expression) = x.type_annotation

let get_t_bool (t:type_value) : unit result = match t.type_value' with
  | T_constant ("bool", []) -> ok ()
  | _ -> simple_fail "not a bool"

let get_t_option (t:type_value) : type_value result = match t.type_value' with
  | T_constant ("option", [o]) -> ok o
  | _ -> simple_fail "not a option"

let get_t_list (t:type_value) : type_value result = match t.type_value' with
  | T_constant ("list", [o]) -> ok o
  | _ -> simple_fail "not a list"

let get_t_tuple (t:type_value) : type_value list result = match t.type_value' with
  | T_tuple lst -> ok lst
  | _ -> simple_fail "not a tuple"

let get_t_sum (t:type_value) : type_value SMap.t result = match t.type_value' with
  | T_sum m -> ok m
  | _ -> simple_fail "not a sum"

let get_t_record (t:type_value) : type_value SMap.t result = match t.type_value' with
  | T_record m -> ok m
  | _ -> simple_fail "not a record"

let get_t_map (t:type_value) : (type_value * type_value) result =
  match t.type_value' with
  | T_constant ("map", [k;v]) -> ok (k, v)
  | _ -> simple_fail "get: not a map"
let assert_t_map (t:type_value) : unit result =
  match t.type_value' with
  | T_constant ("map", [_ ; _]) -> ok ()
  | _ -> simple_fail "not a map"

let assert_t_list (t:type_value) : unit result =
  match t.type_value' with
  | T_constant ("list", [_]) -> ok ()
  | _ -> simple_fail "assert: not a list"

let assert_t_int : type_value -> unit result = fun t -> match t.type_value' with
  | T_constant ("int", []) -> ok ()
  | _ -> simple_fail "not an int"

let assert_t_nat : type_value -> unit result = fun t -> match t.type_value' with
  | T_constant ("nat", []) -> ok ()
  | _ -> simple_fail "not an nat"

let e_record map : expression = E_record map
let ez_e_record (lst : (string * ae) list) : expression =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  e_record map
let e_some s : expression = E_constant ("SOME", [s])
let e_none : expression = E_constant ("NONE", [])

let e_map lst : expression = E_map lst

let e_unit : expression = E_literal (Literal_unit)
let e_int n : expression = E_literal (Literal_int n)
let e_nat n : expression = E_literal (Literal_nat n)
let e_bool b : expression = E_literal (Literal_bool b)
let e_string s : expression = E_literal (Literal_string s)
let e_pair a b : expression = E_tuple [a; b]
let e_list lst : expression = E_list lst

let e_a_unit = make_a_e e_unit (t_unit ())
let e_a_int n = make_a_e (e_int n) (t_int ())
let e_a_nat n = make_a_e (e_nat n) (t_nat ())
let e_a_bool b = make_a_e (e_bool b) (t_bool ())
let e_a_string s = make_a_e (e_string s) (t_string ())
let e_a_pair a b = make_a_e (e_pair a b) (t_pair a.type_annotation b.type_annotation ())
let e_a_some s = make_a_e (e_some s) (t_option s.type_annotation ())
let e_a_none t = make_a_e e_none (t_option t ())
let e_a_tuple lst = make_a_e (E_tuple lst) (t_tuple (List.map get_type_annotation lst) ())
let e_a_record r = make_a_e (e_record r) (t_record (SMap.map get_type_annotation r) ())
let ez_e_a_record r = make_a_e (ez_e_record r) (ez_t_record (List.map (fun (x, y) -> x, y.type_annotation) r) ())
let e_a_map lst k v = make_a_e (e_map lst) (t_map k v ())
let e_a_list lst t = make_a_e (e_list lst) (t_list t ())

let e_a_empty_unit = e_a_unit Environment.full_empty
let e_a_empty_int n = e_a_int n Environment.full_empty
let e_a_empty_nat n = e_a_nat n Environment.full_empty
let e_a_empty_bool b = e_a_bool b Environment.full_empty
let e_a_empty_string s = e_a_string s Environment.full_empty
let e_a_empty_pair a b = e_a_pair a b Environment.full_empty
let e_a_empty_some s = e_a_some s Environment.full_empty
let e_a_empty_none t = e_a_none t Environment.full_empty
let e_a_empty_tuple lst = e_a_tuple lst Environment.full_empty
let e_a_empty_record r = e_a_record r Environment.full_empty
let e_a_empty_map lst k v = e_a_map lst k v Environment.full_empty
let e_a_empty_list lst t = e_a_list lst t Environment.full_empty
let ez_e_a_empty_record r = ez_e_a_record r Environment.full_empty

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

open Environment
let env_sum_type ?(env = full_empty)
    ?(name = "a_sum_type")
    (lst : (string * type_value) list) =
  add_type name (make_t_ez_sum lst) env


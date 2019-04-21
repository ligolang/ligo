open Types

module SMap = Map.String

let t_bool      : type_expression = T_constant ("bool", [])
let t_string    : type_expression = T_constant ("string", [])
let t_bytes     : type_expression = T_constant ("bytes", [])
let t_int       : type_expression = T_constant ("int", [])
let t_unit      : type_expression = T_constant ("unit", [])
let t_option  o : type_expression = T_constant ("option", [o])
let t_list  t : type_expression = T_constant ("list", [t])
let t_tuple lst : type_expression = T_tuple lst
let t_pair a b = t_tuple [a ; b]
let t_record m  : type_expression = (T_record m)
let t_ez_record (lst:(string * type_expression) list) : type_expression =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  T_record map

let t_record_ez lst =
  let m = SMap.of_list lst in
  t_record m

let t_sum m : type_expression = T_sum m
let ez_t_sum (lst:(string * type_expression) list) : type_expression =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  T_sum map

let t_function param result : type_expression = T_function (param, result)

let make_e_a ?type_annotation expression = {expression ; type_annotation}
let make_e_a_full expression type_annotation = make_e_a ~type_annotation expression

let name (s : string) : name = s

let e_var (s : string) : expression = E_variable s

let e_unit  () : expression = E_literal (Literal_unit)
let e_int n : expression = E_literal (Literal_int n)
let e_nat n : expression = E_literal (Literal_nat n)
let e_bool   b : expression = E_literal (Literal_bool b)
let e_string s : expression = E_literal (Literal_string s)
let e_bytes  b : expression = E_literal (Literal_bytes (Bytes.of_string b))

let e_lambda (binder : string)
    (input_type : type_expression)
    (output_type : type_expression)
    (result : expression)
    (body : block)
  : expression =
  E_lambda {
    binder = (name binder) ;
    input_type = input_type ;
    output_type = output_type ;
    result = (make_e_a result) ;
    body ;
  }

let e_tuple (lst : ae list) : expression = E_tuple lst
let ez_e_tuple (lst : expression list) : expression =
  e_tuple (List.map make_e_a lst)

let e_constructor (s : string) (e : ae) : expression = E_constructor (name s, e)

let e_record (lst : (string * ae) list) : expression =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  E_record map

let ez_e_record  (lst : (string * expression) list) : expression =
  (* TODO: define a correct implementation of List.map
   * (an implementation that does not fail with stack overflow) *)
  e_record (List.map (fun (s,e) -> (s, make_e_a e)) lst)

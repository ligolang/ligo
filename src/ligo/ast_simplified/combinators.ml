open Types
open Trace

module SMap = Map.String

let get_name : named_expression -> string = fun x -> x.name
let get_type_name : named_type_expression -> string = fun x -> x.type_name
let get_type_annotation (x:annotated_expression) = x.type_annotation
let get_expression (x:annotated_expression) = x.expression

let i_assignment : _ -> instruction = fun x -> I_assignment x
let named_expression name annotated_expression = { name ; annotated_expression }
let named_typed_expression name expression ty = { name ; annotated_expression = { expression ; type_annotation = Some ty } }
let typed_expression expression ty = { expression ; type_annotation = Some ty }
let untyped_expression expression = { expression ; type_annotation = None }

let get_untyped_expression : annotated_expression -> expression result = fun ae ->
  let%bind () =
    trace_strong (simple_error "expression is typed") @@
    Assert.assert_none ae.type_annotation in
  ok ae.expression

let t_bool      : type_expression = T_constant ("bool", [])
let t_string    : type_expression = T_constant ("string", [])
let t_bytes     : type_expression = T_constant ("bytes", [])
let t_int       : type_expression = T_constant ("int", [])
let t_operation : type_expression = T_constant ("operation", [])
let t_nat       : type_expression = T_constant ("nat", [])
let t_tez       : type_expression = T_constant ("tez", [])
let t_unit      : type_expression = T_constant ("unit", [])
let t_address      : type_expression = T_constant ("address", [])
let t_option  o : type_expression = T_constant ("option", [o])
let t_list  t : type_expression = T_constant ("list", [t])
let t_variable n : type_expression = T_variable n
let t_tuple lst : type_expression = T_tuple lst
let t_pair (a , b) = t_tuple [a ; b]
let t_record m  : type_expression = (T_record m)

let t_record_ez lst =
  let m = SMap.of_list lst in
  t_record m

let t_sum m : type_expression = T_sum m
let ez_t_sum (lst:(string * type_expression) list) : type_expression =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  T_sum map

let t_function param result : type_expression = T_function (param, result)
let t_map key value = (T_constant ("map", [key ; value]))

let make_e_a ?type_annotation expression = {expression ; type_annotation}
let make_e_a_full expression type_annotation = make_e_a ~type_annotation expression

let make_name (s : string) : name = s

let e_var (s : string) : expression = E_variable s

let e_unit () : expression = E_literal (Literal_unit)
let e_int n : expression = E_literal (Literal_int n)
let e_nat n : expression = E_literal (Literal_nat n)
let e_bool   b : expression = E_literal (Literal_bool b)
let e_string s : expression = E_literal (Literal_string s)
let e_address s : expression = E_literal (Literal_address s)
let e_tez s : expression = E_literal (Literal_tez s)
let e_bytes  b : expression = E_literal (Literal_bytes (Bytes.of_string b))
let e_record map : expression = E_record map
let e_tuple lst : expression = E_tuple lst
let e_some s : expression = E_constant ("SOME", [s])
let e_none : expression = E_constant ("NONE", [])
let e_map lst : expression = E_map lst
let e_list lst : expression = E_list lst
let e_pair a b : expression = E_tuple [a; b]
let e_constructor s a : expression = E_constructor (s , a)
let e_match a b : expression = E_matching (a , b)
let e_match_bool a b c : expression = e_match a (Match_bool {match_true = b ; match_false = c})
let e_accessor a b = E_accessor (a , b)
let e_accessor_props a b = e_accessor a (List.map (fun x -> Access_record x) b)
let e_variable v = E_variable v
let e_failwith v = E_failwith v

let e_a_unit : annotated_expression = make_e_a_full (e_unit ()) t_unit
let e_a_string s : annotated_expression = make_e_a_full (e_string s) t_string
let e_a_int n : annotated_expression = make_e_a_full (e_int n) t_int
let e_a_nat n : annotated_expression = make_e_a_full (e_nat n) t_nat
let e_a_bool b : annotated_expression = make_e_a_full (e_bool b) t_bool
let e_a_list lst : annotated_expression = make_e_a (e_list lst)
let e_a_constructor s a : annotated_expression = make_e_a (e_constructor s a)
let e_a_address x = make_e_a_full (e_address x) t_address
let e_a_tez x = make_e_a_full (e_tez x) t_tez

let e_a_record r =
  let type_annotation = Option.(
      map ~f:t_record (bind_map_smap get_type_annotation r)
    ) in
  make_e_a ?type_annotation (e_record r)

let ez_e_a_record lst =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  e_a_record map

let e_a_tuple lst =
  let type_annotation = Option.(
      map ~f:t_tuple (bind_map_list get_type_annotation lst)
    ) in
  make_e_a ?type_annotation (e_tuple lst)

let e_a_pair a b =
  let type_annotation = Option.(
      map ~f:t_pair
      @@ bind_map_pair get_type_annotation (a , b)
    ) in
  make_e_a ?type_annotation (e_pair a b)

let e_a_some opt =
  let type_annotation = Option.(
      map ~f:t_option (get_type_annotation opt)
    ) in
  make_e_a ?type_annotation (e_some opt)

let e_a_typed_none t_opt =
  let type_annotation = t_option t_opt in
  make_e_a ~type_annotation e_none

let e_a_typed_list lst t =
  make_e_a ~type_annotation:(t_list t) (e_list lst)

let e_a_map lst k v = make_e_a ~type_annotation:(t_map k v) (e_map lst)

let e_lambda (binder : string)
    (input_type : type_expression)
    (output_type : type_expression)
    (result : expression)
    (body : block)
  : expression =
  E_lambda {
    binder = (make_name binder) ;
    input_type = input_type ;
    output_type = output_type ;
    result = (make_e_a result) ;
    body ;
  }

let e_tuple (lst : ae list) : expression = E_tuple lst
let ez_e_tuple (lst : expression list) : expression =
  e_tuple (List.map make_e_a lst)

let e_constructor (s : string) (e : ae) : expression = E_constructor (make_name s, e)

let e_record (lst : (string * ae) list) : expression =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  E_record map

let ez_e_record  (lst : (string * expression) list) : expression =
  (* TODO: define a correct implementation of List.map
   * (an implementation that does not fail with stack overflow) *)
  e_record (List.map (fun (s,e) -> (s, make_e_a e)) lst)


let get_a_accessor = fun t ->
  match t.expression with
  | E_accessor (a , b) -> ok (a , b)
  | _ -> simple_fail "not an accessor"

let assert_a_accessor = fun t ->
  let%bind _ = get_a_accessor t in
  ok ()

let get_access_record : access -> string result = fun a ->
  match a with
  | Access_tuple _
  | Access_map _ -> simple_fail "not an access record"
  | Access_record s -> ok s

let get_a_pair = fun t ->
  match t.expression with
  | E_tuple [a ; b] -> ok (a , b)
  | _ -> simple_fail "not a pair"

let get_a_list = fun t ->
  match t.expression with
  | E_list lst -> ok lst
  | _ -> simple_fail "not a pair"

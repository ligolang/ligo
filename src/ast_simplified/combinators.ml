open Types
open Simple_utils.Trace
module Option = Simple_utils.Option

module SMap = Map.String

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
let e_map_update k v old : expression = E_constant ("MAP_UPDATE" , [k ; v ; old])
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
let e_skip = E_skip
let e_loop cond body = E_loop (cond , body)
let e_sequence a b = E_sequence (a , b)
let e_let_in binder rhs result = E_let_in { binder ; rhs ; result }
let e_annotation expr ty = E_annotation (expr , ty)
let e_application a b = E_application (a , b)

let e_binop name a b = E_constant (name , [a ; b])

let make_option_typed e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation e t


let ez_e_record lst =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  e_record map

let e_typed_none t_opt =
  let type_annotation = t_option t_opt in
  e_annotation e_none type_annotation

let e_typed_list lst t =
  e_annotation (e_list lst) (t_list t)

let e_map lst k v = e_annotation (e_map lst) (t_map k v)

let e_lambda (binder : string)
    (input_type : type_expression option)
    (output_type : type_expression option)
    (result : expression)
  : expression =
  E_lambda {
    binder = (make_name binder , input_type) ;
    input_type = input_type ;
    output_type = output_type ;
    result ;
  }

let e_record (lst : (string * expr) list) : expression =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  E_record map

let get_e_accessor = fun t ->
  match t with
  | E_accessor (a , b) -> ok (a , b)
  | _ -> simple_fail "not an accessor"

let assert_e_accessor = fun t ->
  let%bind _ = get_e_accessor t in
  ok ()

let get_access_record : access -> string result = fun a ->
  match a with
  | Access_tuple _
  | Access_map _ -> simple_fail "not an access record"
  | Access_record s -> ok s

let get_e_pair = fun t ->
  match t with
  | E_tuple [a ; b] -> ok (a , b)
  | _ -> simple_fail "not a pair"

let get_e_list = fun t ->
  match t with
  | E_list lst -> ok lst
  | _ -> simple_fail "not a pair"

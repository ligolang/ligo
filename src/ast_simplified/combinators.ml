open Types
open Simple_utils.Trace
module Option = Simple_utils.Option

module SMap = Map.String

module Errors = struct
  let bad_kind expected location =
    let title () = Format.asprintf "a %s was expected" expected in
    let message () = "" in
    let data = [
      ("location" , fun () -> Format.asprintf "%a" Location.pp location) ;
    ] in
    error ~data title message
end
open Errors

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
let t_set key = (T_constant ("set", [key]))

let make_name (s : string) : name = s

let e_var ?loc (s : string) : expression = Location.wrap ?loc @@ E_variable s
let e_literal ?loc l : expression = Location.wrap ?loc @@ E_literal l
let e_unit ?loc () : expression = Location.wrap ?loc @@ E_literal (Literal_unit)
let e_int ?loc n : expression = Location.wrap ?loc @@ E_literal (Literal_int n)
let e_nat ?loc n : expression = Location.wrap ?loc @@ E_literal (Literal_nat n)
let e_timestamp ?loc n : expression = Location.wrap ?loc @@ E_literal (Literal_timestamp n)
let e_bool ?loc   b : expression = Location.wrap ?loc @@ E_literal (Literal_bool b)
let e_string ?loc s : expression = Location.wrap ?loc @@ E_literal (Literal_string s)
let e_address ?loc s : expression = Location.wrap ?loc @@ E_literal (Literal_address s)
let e_tez ?loc s : expression = Location.wrap ?loc @@ E_literal (Literal_tez s)
let e_bytes ?loc  b : expression = Location.wrap ?loc @@ E_literal (Literal_bytes (Bytes.of_string b))
let e_record ?loc map : expression = Location.wrap ?loc @@ E_record map
let e_tuple ?loc lst : expression = Location.wrap ?loc @@ E_tuple lst
let e_some ?loc s : expression = Location.wrap ?loc @@ E_constant ("SOME", [s])
let e_none ?loc () : expression = Location.wrap ?loc @@ E_constant ("NONE", [])
let e_map_add ?loc k v old : expression = Location.wrap ?loc @@ E_constant ("MAP_ADD" , [k ; v ; old])
let e_map ?loc lst : expression = Location.wrap ?loc @@ E_map lst
let e_set ?loc lst : expression = Location.wrap ?loc @@ E_set lst
let e_list ?loc lst : expression = Location.wrap ?loc @@ E_list lst
let e_pair ?loc a b : expression = Location.wrap ?loc @@ E_tuple [a; b]
let e_constructor ?loc s a : expression = Location.wrap ?loc @@ E_constructor (s , a)
let e_matching ?loc a b : expression = Location.wrap ?loc @@ E_matching (a , b)
let e_matching_bool ?loc a b c : expression = e_matching ?loc a (Match_bool {match_true = b ; match_false = c})
let e_accessor ?loc a b = Location.wrap ?loc @@ E_accessor (a , b)
let e_accessor_props ?loc a b = e_accessor ?loc a (List.map (fun x -> Access_record x) b)
let e_variable ?loc v = Location.wrap ?loc @@ E_variable v
let e_failwith ?loc v = Location.wrap ?loc @@ E_failwith v
let e_skip ?loc () = Location.wrap ?loc @@ E_skip
let e_loop ?loc cond body = Location.wrap ?loc @@ E_loop (cond , body)
let e_sequence ?loc a b = Location.wrap ?loc @@ E_sequence (a , b)
let e_let_in ?loc binder rhs result = Location.wrap ?loc @@ E_let_in { binder ; rhs ; result }
let e_annotation ?loc expr ty = Location.wrap ?loc @@ E_annotation (expr , ty)
let e_application ?loc a b = Location.wrap ?loc @@ E_application (a , b)
let e_binop ?loc name a b = Location.wrap ?loc @@ E_constant (name , [a ; b])
let e_constant ?loc name lst = Location.wrap ?loc @@ E_constant (name , lst)
let e_look_up ?loc x y = Location.wrap ?loc @@ E_look_up (x , y)
let e_assign ?loc a b c = Location.wrap ?loc @@ E_assign (a , b , c)

let make_option_typed ?loc e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc e t


let ez_e_record ?loc lst =
  let aux prev (k, v) = SMap.add k v prev in
  let map = List.fold_left aux SMap.empty lst in
  e_record ?loc map

let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation

let e_typed_list ?loc lst t =
  e_annotation ?loc (e_list lst) (t_list t)

let e_typed_map ?loc lst k v = e_annotation ?loc (e_map lst) (t_map k v)
    
let e_typed_set ?loc lst k = e_annotation ?loc (e_set lst) (t_set k)

let e_lambda ?loc (binder : string)
    (input_type : type_expression option)
    (output_type : type_expression option)
    (result : expression)
  : expression =
  Location.wrap ?loc @@ E_lambda {
    binder = (make_name binder , input_type) ;
    input_type = input_type ;
    output_type = output_type ;
    result ;
  }

let e_record ?loc map = Location.wrap ?loc @@ E_record map

let e_ez_record ?loc (lst : (string * expr) list) : expression =
  let map = SMap.of_list lst in
  e_record ?loc map

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

let get_e_failwith = fun e ->
  match Location.unwrap e with
  | E_failwith fw -> ok fw
  | _ -> simple_fail "not a failwith"

let is_e_failwith e = to_bool @@ get_e_failwith e

let extract_pair : expression -> (expression * expression) result = fun e ->
  match Location.unwrap e with
  | E_tuple [ a ; b ] -> ok (a , b)
  | _ -> fail @@ bad_kind "pair" e.location

let extract_list : expression -> (expression list) result = fun e ->
  match Location.unwrap e with
  | E_list lst -> ok lst
  | _ -> fail @@ bad_kind "list" e.location

let extract_record : expression -> (string * expression) list result = fun e ->
  match Location.unwrap e with
  | E_record lst -> ok @@ SMap.to_kv_list lst
  | _ -> fail @@ bad_kind "record" e.location

let extract_map : expression -> (expression * expression) list result = fun e ->
  match Location.unwrap e with
  | E_map lst -> ok lst
  | _ -> fail @@ bad_kind "map" e.location

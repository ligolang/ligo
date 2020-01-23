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
  let bad_type_operator type_op =
    let title () = Format.asprintf "bad type operator %a" (Stage_common.PP.type_operator PP.type_expression) type_op in
    let message () = "" in
    error title message
end
open Errors

let make_t type_expression' = {type_expression'}

let t_bool         : type_expression = make_t @@ T_constant (TC_bool)
let t_string       : type_expression = make_t @@ T_constant (TC_string)
let t_bytes        : type_expression = make_t @@ T_constant (TC_bytes)
let t_int          : type_expression = make_t @@ T_constant (TC_int)
let t_operation    : type_expression = make_t @@ T_constant (TC_operation)
let t_nat          : type_expression = make_t @@ T_constant (TC_nat)
let t_tez          : type_expression = make_t @@ T_constant (TC_mutez)
let t_unit         : type_expression = make_t @@ T_constant (TC_unit)
let t_address      : type_expression = make_t @@ T_constant (TC_address)
let t_signature    : type_expression = make_t @@ T_constant (TC_signature)
let t_key          : type_expression = make_t @@ T_constant (TC_key)
let t_key_hash     : type_expression = make_t @@ T_constant (TC_key_hash)
let t_option  o    : type_expression = make_t @@ T_operator (TC_option o)
let t_list  t      : type_expression = make_t @@ T_operator (TC_list t)
let t_variable n   : type_expression = make_t @@ T_variable (Var.of_name n)
let t_tuple lst    : type_expression = make_t @@ T_operator (TC_tuple lst)
let t_pair (a , b) : type_expression = t_tuple [a ; b]
let t_record_ez lst =
  let lst = List.map (fun (k, v) -> (Label k, v)) lst in
  let m = LMap.of_list lst in
  make_t @@ T_record m
let t_record m  : type_expression =
  let lst = Map.String.to_kv_list m in
  t_record_ez lst

let ez_t_sum (lst:(string * type_expression) list) : type_expression =
  let aux prev (k, v) = CMap.add (Constructor k) v prev in
  let map = List.fold_left aux CMap.empty lst in
  make_t @@ T_sum map
let t_sum m : type_expression =
  let lst = Map.String.to_kv_list m in
  ez_t_sum lst

let t_function param result : type_expression = make_t @@ T_arrow (param, result)
let t_map key value         : type_expression = make_t @@ T_operator (TC_map (key, value))
let t_big_map key value     : type_expression = make_t @@ T_operator (TC_big_map (key , value))
let t_set key               : type_expression = make_t @@ T_operator (TC_set key)
let t_contract contract     : type_expression = make_t @@ T_operator (TC_contract contract)

(* TODO find a better way than using list*)
let t_operator op lst: type_expression result =
  match op,lst with 
  | TC_set _         , [t] -> ok @@ t_set t
  | TC_list _        , [t] -> ok @@ t_list t
  | TC_option _      , [t] -> ok @@ t_option t
  | TC_map (_,_)     , [kt;vt] -> ok @@ t_map kt vt
  | TC_big_map (_,_) , [kt;vt] -> ok @@ t_big_map kt vt
  | TC_contract _    , [t] -> ok @@ t_contract t
  | _ , _ -> fail @@ bad_type_operator op

let location_wrap ?(loc = Location.generated) expression =
  let location = loc in
  { location ; expression }

let e_var ?loc (n: string) : expression = location_wrap ?loc @@ E_variable (Var.of_name n)
let e_literal ?loc l : expression = location_wrap ?loc @@ E_literal l
let e_unit ?loc () : expression = location_wrap ?loc @@ E_literal (Literal_unit)
let e_int ?loc n : expression = location_wrap ?loc @@ E_literal (Literal_int n)
let e_nat ?loc n : expression = location_wrap ?loc @@ E_literal (Literal_nat n)
let e_timestamp ?loc n : expression = location_wrap ?loc @@ E_literal (Literal_timestamp n)
let e_bool ?loc   b : expression = location_wrap ?loc @@ E_literal (Literal_bool b)
let e_string ?loc s : expression = location_wrap ?loc @@ E_literal (Literal_string s)
let e_address ?loc s : expression = location_wrap ?loc @@ E_literal (Literal_address s)
let e_mutez ?loc s : expression = location_wrap ?loc @@ E_literal (Literal_mutez s)
let e_signature ?loc s : expression = location_wrap ?loc @@ E_literal (Literal_signature s)
let e_key ?loc s : expression = location_wrap ?loc @@ E_literal (Literal_key s)
let e_key_hash ?loc s : expression = location_wrap ?loc @@ E_literal (Literal_key_hash s)
let e_chain_id ?loc s : expression = location_wrap ?loc @@ E_literal (Literal_chain_id s)
let e'_bytes b : expression' result =
  let%bind bytes = generic_try (simple_error "bad hex to bytes") (fun () -> Hex.to_bytes (`Hex b)) in
  ok @@ E_literal (Literal_bytes bytes)
let e_bytes_hex ?loc b : expression result =
  let%bind e' = e'_bytes b in
  ok @@ location_wrap ?loc e'
let e_bytes_raw ?loc (b: bytes) : expression =
  location_wrap ?loc @@ E_literal (Literal_bytes b)
let e_bytes_string ?loc (s: string) : expression =
  location_wrap ?loc @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))
let e_big_map ?loc lst : expression = location_wrap ?loc @@ E_big_map lst
let e_record ?loc map : expression = location_wrap ?loc @@ E_record map
let e_tuple ?loc lst : expression = location_wrap ?loc @@ E_tuple lst
let e_some ?loc s  : expression = location_wrap ?loc @@ E_constant (C_SOME, [s])
let e_none ?loc () : expression = location_wrap ?loc @@ E_constant (C_NONE, [])
let e_string_cat ?loc sl sr : expression = location_wrap ?loc @@ E_constant (C_CONCAT, [sl ; sr ])
let e_map_add ?loc k v old  : expression = location_wrap ?loc @@ E_constant (C_MAP_ADD, [k ; v ; old])
let e_map ?loc lst : expression = location_wrap ?loc @@ E_map lst
let e_set ?loc lst : expression = location_wrap ?loc @@ E_set lst
let e_list ?loc lst : expression = location_wrap ?loc @@ E_list lst
let e_pair ?loc a b : expression = location_wrap ?loc @@ E_tuple [a; b]
let e_constructor ?loc s a : expression = location_wrap ?loc @@ E_constructor (Constructor s , a)
let e_matching ?loc a b : expression = location_wrap ?loc @@ E_matching (a , b)
let e_matching_bool ?loc a b c : expression = e_matching ?loc a (Match_bool {match_true = b ; match_false = c})
let e_accessor ?loc a b = location_wrap ?loc @@ E_accessor (a , b)
let e_accessor_props ?loc a b = e_accessor ?loc a (List.map (fun x -> Access_record x) b)
let e_variable ?loc v = location_wrap ?loc @@ E_variable v
let e_skip ?loc () = location_wrap ?loc @@ E_skip
let e_loop ?loc cond body = location_wrap ?loc @@ E_loop (cond , body)
let e_sequence ?loc a b = location_wrap ?loc @@ E_sequence (a , b)
let e_let_in ?loc (binder, ascr) inline rhs result = location_wrap ?loc @@ E_let_in { binder = (binder, ascr) ; rhs ; result ; inline }
let e_annotation ?loc expr ty = location_wrap ?loc @@ E_ascription (expr , ty)
let e_application ?loc a b = location_wrap ?loc @@ E_application (a , b)
let e_binop ?loc name a b  = location_wrap ?loc @@ E_constant (name , [a ; b])
let e_constant ?loc name lst = location_wrap ?loc @@ E_constant (name , lst)
let e_look_up ?loc x y = location_wrap ?loc @@ E_look_up (x , y)
let e_assign ?loc a b c = location_wrap ?loc @@ E_assign (Var.of_name a , b , c)
let ez_match_variant (lst : ((string * string) * 'a) list) =
  let lst = List.map (fun ((c,n),a) -> ((Constructor c, Var.of_name n), a) ) lst in
  Match_variant (lst,())
let e_matching_variant ?loc a (lst : ((string * string)* 'a) list) =
  e_matching ?loc a (ez_match_variant lst)

let make_option_typed ?loc e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc e t


let ez_e_record ?loc (lst : (string * expr) list) =
  let aux prev (k, v) = LMap.add k v prev in
  let lst = List.map (fun (k,v) -> (Label k, v)) lst in
  let map = List.fold_left aux LMap.empty lst in
  e_record ?loc map

let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation

let e_typed_list ?loc lst t =
  e_annotation ?loc (e_list lst) (t_list t)

let e_typed_map ?loc lst k v = e_annotation ?loc (e_map lst) (t_map k v)
let e_typed_big_map ?loc lst k v = e_annotation ?loc (e_big_map lst) (t_big_map k v)

let e_typed_set ?loc lst k = e_annotation ?loc (e_set lst) (t_set k)

let e_lambda ?loc (binder : expression_variable)
    (input_type : type_expression option)
    (output_type : type_expression option)
    (result : expression)
  : expression =
  location_wrap ?loc @@ E_lambda {
    binder = (binder , input_type) ;
    input_type = input_type ;
    output_type = output_type ;
    result ;
  }

let e_ez_record ?loc (lst : (string * expr) list) : expression =
  let map = List.fold_left (fun m (x, y) -> LMap.add (Label x) y m) LMap.empty lst in
  location_wrap ?loc @@ E_record map
let e_record ?loc map =
  let lst = Map.String.to_kv_list map in
  e_ez_record ?loc lst 
let e_update ?loc record updates = 
  let updates = List.map (fun (x,y) -> (Label x, y)) updates in
  location_wrap ?loc @@ E_update {record; updates}

let get_e_accessor = fun t ->
  match t with
  | E_accessor (a , b) -> ok (a , b)
  | _ -> simple_fail "not an accessor"

let assert_e_accessor = fun t ->
  let%bind _ = get_e_accessor t in
  ok ()

let get_access_record : access -> string result = fun a ->
  match a with
  | Access_tuple _ -> simple_fail "not an access record"
  | Access_record s -> ok s

let get_e_pair = fun t ->
  match t with
  | E_tuple [a ; b] -> ok (a , b)
  | _ -> simple_fail "not a pair"

let get_e_list = fun t ->
  match t with
  | E_list lst -> ok lst
  | _ -> simple_fail "not a list"

let get_e_tuple = fun t ->
  match t with
  | E_tuple lst -> ok lst
  | _ -> simple_fail "ast_simplified: get_e_tuple: not a tuple"

let extract_pair : expression -> (expression * expression) result = fun e ->
  match e.expression with
  | E_tuple [ a ; b ] -> ok (a , b)
  | _ -> fail @@ bad_kind "pair" e.location

let extract_list : expression -> (expression list) result = fun e ->
  match e.expression with
  | E_list lst -> ok lst
  | _ -> fail @@ bad_kind "list" e.location

let extract_record : expression -> (label * expression) list result = fun e ->
  match e.expression with
  | E_record lst -> ok @@ LMap.to_kv_list lst
  | _ -> fail @@ bad_kind "record" e.location

let extract_map : expression -> (expression * expression) list result = fun e ->
  match e.expression with
  | E_map lst -> ok lst
  | _ -> fail @@ bad_kind "map" e.location

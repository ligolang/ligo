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

let make_t ?(loc = Location.generated) type_content = {type_content; location=loc; type_meta = ()}

  
let tuple_to_record lst =
  let aux (i,acc) el = (i+1,(string_of_int i, el)::acc) in
  let (_, lst ) = List.fold_left aux (0,[]) lst in
  lst

let t_bool ?loc ()        : type_expression = make_t ?loc @@ T_variable (Stage_common.Constant.t_bool)
let t_string ?loc ()      : type_expression = make_t ?loc @@ T_constant (TC_string)
let t_bytes ?loc ()       : type_expression = make_t ?loc @@ T_constant (TC_bytes)
let t_int ?loc ()         : type_expression = make_t ?loc @@ T_constant (TC_int)
let t_operation ?loc ()   : type_expression = make_t ?loc @@ T_constant (TC_operation)
let t_nat ?loc ()         : type_expression = make_t ?loc @@ T_constant (TC_nat)
let t_tez ?loc ()         : type_expression = make_t ?loc @@ T_constant (TC_mutez)
let t_unit ?loc ()        : type_expression = make_t ?loc @@ T_constant (TC_unit)
let t_address ?loc ()     : type_expression = make_t ?loc @@ T_constant (TC_address)
let t_signature ?loc ()   : type_expression = make_t ?loc @@ T_constant (TC_signature)
let t_key ?loc ()         : type_expression = make_t ?loc @@ T_constant (TC_key)
let t_key_hash ?loc ()    : type_expression = make_t ?loc @@ T_constant (TC_key_hash)
let t_timestamp ?loc ()   : type_expression = make_t ?loc @@ T_constant (TC_timestamp)
let t_option ?loc o       : type_expression = make_t ?loc @@ T_operator (TC_option, [o])
let t_list ?loc t         : type_expression = make_t ?loc @@ T_operator (TC_list, [t])
let t_variable ?loc n     : type_expression = make_t ?loc @@ T_variable (Var.of_name n)
let t_record_ez ?loc lst =
  let lst = List.map (fun (k, v) -> (Label k, v)) lst in
  let m = LMap.of_list lst in
  make_t ?loc @@ T_record m
let t_record ?loc m  : type_expression =
  let lst = Map.String.to_kv_list m in
  t_record_ez ?loc lst

let t_pair ?loc (a , b) : type_expression = t_record_ez ?loc [("0",a) ; ("1",b)]
let t_tuple ?loc lst    : type_expression = t_record_ez ?loc (tuple_to_record lst)

let ez_t_sum ?loc (lst:(string * ctor_content) list) : type_expression =
  let aux prev (k, v) = CMap.add (Constructor k) v prev in
  let map = List.fold_left aux CMap.empty lst in
  make_t ?loc @@ T_sum map
let t_sum ?loc m : type_expression =
  let lst = Map.String.to_kv_list m in
  ez_t_sum ?loc lst

let t_function ?loc type1 type2  : type_expression = make_t ?loc @@ T_arrow {type1; type2}

let t_operator ?loc op lst       : type_expression = make_t ?loc @@ T_operator (op, lst)
let t_map ?loc key value         : type_expression = make_t ?loc @@ T_operator (TC_map, [key; value])
let t_big_map ?loc key value     : type_expression = make_t ?loc @@ T_operator (TC_big_map, [key; value])
let t_set ?loc key               : type_expression = make_t ?loc @@ T_operator (TC_set, [key])
let t_contract ?loc contract     : type_expression = make_t ?loc @@ T_operator (TC_contract, [contract])

let make_e ?(loc = Location.generated) expression_content = { expression_content; location=loc }

let e_var ?loc (n: string) : expression = make_e ?loc @@ E_variable (Var.of_name n)
let e_literal ?loc l : expression = make_e ?loc @@ E_literal l
let e_unit ?loc () : expression = make_e ?loc @@ E_literal (Literal_unit)
let e_int ?loc n : expression = make_e ?loc @@ E_literal (Literal_int n)
let e_nat ?loc n : expression = make_e ?loc @@ E_literal (Literal_nat n)
let e_timestamp ?loc n : expression = make_e ?loc @@ E_literal (Literal_timestamp n)
let e_string ?loc s : expression = make_e ?loc @@ E_literal (Literal_string s)
let e_address ?loc s : expression = make_e ?loc @@ E_literal (Literal_address s)
let e_mutez ?loc s : expression = make_e ?loc @@ E_literal (Literal_mutez s)
let e_signature ?loc s : expression = make_e ?loc @@ E_literal (Literal_signature s)
let e_key ?loc s : expression = make_e ?loc @@ E_literal (Literal_key s)
let e_key_hash ?loc s : expression = make_e ?loc @@ E_literal (Literal_key_hash s)
let e_chain_id ?loc s : expression = make_e ?loc @@ E_literal (Literal_chain_id s)
let e'_bytes b : expression_content result =
  let%bind bytes = generic_try (simple_error "bad hex to bytes") (fun () -> Hex.to_bytes (`Hex b)) in
  ok @@ E_literal (Literal_bytes bytes)
let e_bytes_hex ?loc b : expression result =
  let%bind e' = e'_bytes b in
  ok @@ make_e ?loc e'
let e_bytes_raw ?loc (b: bytes) : expression =
  make_e ?loc @@ E_literal (Literal_bytes b)
let e_bytes_string ?loc (s: string) : expression =
  make_e ?loc @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))
let e_some ?loc s  : expression = make_e ?loc @@ E_constant {cons_name = C_SOME; arguments = [s]}
let e_none ?loc () : expression = make_e ?loc @@ E_constant {cons_name = C_NONE; arguments = []}
let e_string_cat ?loc sl sr : expression = make_e ?loc @@ E_constant {cons_name = C_CONCAT; arguments = [sl ; sr ]}
let e_map_add ?loc k v old  : expression = make_e ?loc @@ E_constant {cons_name = C_MAP_ADD; arguments = [k ; v ; old]}

let e_constant ?loc name lst = make_e ?loc @@ E_constant {cons_name=name ; arguments = lst}
let e_variable ?loc v = make_e ?loc @@ E_variable v
let e_application ?loc a b = make_e ?loc @@ E_application {lamb=a ; args=b}
let e_lambda ?loc binder input_type output_type result =  make_e ?loc @@ E_lambda {binder; input_type; output_type; result ;  }
let e_recursive ?loc fun_name fun_type lambda = make_e ?loc @@ E_recursive {fun_name; fun_type; lambda}
let e_let_in ?loc (binder, ascr) inline rhs let_result = make_e ?loc @@ E_let_in { let_binder = (binder,ascr) ; rhs ; let_result; inline }

let e_constructor ?loc s a : expression = make_e ?loc @@ E_constructor { constructor = Constructor s; element = a}
let e_matching ?loc a b : expression = make_e ?loc @@ E_matching {matchee=a;cases=b}

let e_record ?loc map = make_e ?loc @@ E_record map
let e_record_accessor ?loc a b = make_e ?loc @@ E_record_accessor {record = a; path = b}
let e_record_update ?loc record path update = make_e ?loc @@ E_record_update {record; path; update}

let e_annotation ?loc anno_expr ty = make_e ?loc @@ E_ascription {anno_expr; type_annotation = ty}

let e_bool ?loc   b : expression = e_constructor ?loc (string_of_bool b) (e_unit ())

let make_option_typed ?loc e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc e t

let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation



let get_e_record_accessor = fun t ->
  match t with
  | E_record_accessor {record; path} -> ok (record, path)
  | _ -> simple_fail "not an accessor"

let assert_e_record_accessor = fun t ->
  let%bind _ = get_e_record_accessor t in
  ok ()

let get_e_pair = fun t ->
  match t with
  | E_record r -> ( 
  let lst = LMap.to_kv_list r in
    match lst with 
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] -> 
        ok (a , b)
    | _ -> simple_fail "not a pair"
    )
  | _ -> simple_fail "not a pair"

let get_e_list = fun t ->
  let rec aux t = 
    match t with
      E_constant {cons_name=C_CONS;arguments=[key;lst]} -> 
        let%bind lst = aux lst.expression_content in
        ok @@ key::(lst)
    | E_constant {cons_name=C_LIST_EMPTY;arguments=[]} ->
        ok @@ []
    | _ -> simple_fail "not a list"
  in
  aux t

let get_e_tuple = fun t ->
  match t with
  | E_record r -> ok @@ List.map snd @@ Stage_common.Helpers.tuple_of_record r
  | _ -> simple_fail "ast_core: get_e_tuple: not a tuple"

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) result = fun e ->
  match e.expression_content with
  | E_record r -> ( 
  let lst = LMap.to_kv_list r in
    match lst with 
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] -> 
        ok (a , b)
    | _ -> fail @@ bad_kind "pair" e.location
    )
  | _ -> fail @@ bad_kind "pair" e.location

let extract_record : expression -> (label * expression) list result = fun e ->
  match e.expression_content with
  | E_record lst -> ok @@ LMap.to_kv_list lst
  | _ -> fail @@ bad_kind "record" e.location

let extract_map : expression -> (expression * expression) list result = fun e ->
  let rec aux e =
    match e.expression_content with
      E_constant {cons_name=C_UPDATE|C_MAP_ADD; arguments=[k;v;map]} -> 
        let%bind map = aux map in
        ok @@ (k,v)::map 
    | E_constant {cons_name=C_MAP_EMPTY|C_BIG_MAP_EMPTY; arguments=[]} -> ok @@ []
    | _ -> fail @@ bad_kind "map" e.location
  in
  aux e

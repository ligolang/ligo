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
    let title () = Format.asprintf "bad type operator %a" (PP.type_operator PP.type_expression) type_op in
    let message () = "" in
    error title message
end
open Errors

let make_t ?(loc = Location.generated) type_content = {type_content; location=loc}

let t_bool ?loc ()        : type_expression = make_t ?loc @@ T_constant (TC_bool)
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
let t_option ?loc o       : type_expression = make_t ?loc @@ T_operator (TC_option o)
let t_list ?loc t         : type_expression = make_t ?loc @@ T_operator (TC_list t)
let t_variable ?loc n     : type_expression = make_t ?loc @@ T_variable (Var.of_name n)
let t_record_ez ?loc lst =
  let lst = List.map (fun (k, v) -> (Label k, v)) lst in
  let m = LMap.of_list lst in
  make_t ?loc @@ T_record m
let t_record ?loc m  : type_expression =
  let lst = Map.String.to_kv_list m in
  t_record_ez ?loc lst

let t_tuple ?loc lst    : type_expression = make_t ?loc @@ T_tuple lst
let t_pair ?loc (a , b) : type_expression = t_tuple ?loc [a; b]

let ez_t_sum ?loc (lst:(string * type_expression) list) : type_expression =
  let aux prev (k, v) = CMap.add (Constructor k) v prev in
  let map = List.fold_left aux CMap.empty lst in
  make_t ?loc @@ T_sum map
let t_sum ?loc m : type_expression =
  let lst = Map.String.to_kv_list m in
  ez_t_sum ?loc lst

let t_function ?loc type1 type2  : type_expression = make_t ?loc @@ T_arrow {type1; type2}
let t_map ?loc key value         : type_expression = make_t ?loc @@ T_operator (TC_map (key, value))
let t_big_map ?loc key value     : type_expression = make_t ?loc @@ T_operator (TC_big_map (key , value))
let t_set ?loc key               : type_expression = make_t ?loc @@ T_operator (TC_set key)
let t_contract ?loc contract     : type_expression = make_t ?loc @@ T_operator (TC_contract contract)
let t_michelson_or ?loc l l_ann r r_ann : type_expression = make_t ?loc @@ T_operator (TC_michelson_or (l, l_ann, r, r_ann))
let t_michelson_pair ?loc l l_ann r r_ann : type_expression = make_t ?loc @@ T_operator (TC_michelson_pair (l, l_ann, r, r_ann))

(* TODO find a better way than using list*)
let t_operator ?loc op lst: type_expression result =
  match op,lst with 
  | TC_set _         , [t] -> ok @@ t_set ?loc t
  | TC_list _        , [t] -> ok @@ t_list ?loc t
  | TC_option _      , [t] -> ok @@ t_option ?loc t
  | TC_map (_,_)     , [kt;vt] -> ok @@ t_map ?loc kt vt
  | TC_big_map (_,_) , [kt;vt] -> ok @@ t_big_map ?loc kt vt
  | TC_michelson_or (_,l_ann,_,r_ann) , [l;r] -> ok @@ t_michelson_or ?loc l l_ann r r_ann
  | TC_contract _    , [t] -> ok @@ t_contract t
  | _ , _ -> fail @@ bad_type_operator op

let make_e ?(loc = Location.generated) expression_content =
  let location = loc in
  { expression_content; location }

let e_literal ?loc l : expression = make_e ?loc @@ E_literal l
let e_unit ?loc () : expression = make_e ?loc @@ E_literal (Literal_unit)
let e_int ?loc n : expression = make_e ?loc @@ E_literal (Literal_int n)
let e_nat ?loc n : expression = make_e ?loc @@ E_literal (Literal_nat n)
let e_timestamp ?loc n : expression = make_e ?loc @@ E_literal (Literal_timestamp n)
let e_bool ?loc   b : expression = make_e ?loc @@ E_literal (Literal_bool b)
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
let e_binop ?loc name a b  = make_e ?loc @@ E_constant {cons_name = name ; arguments = [a ; b]}

let e_constant ?loc name lst = make_e ?loc @@ E_constant {cons_name=name ; arguments = lst}
let e_variable ?loc v = make_e ?loc @@ E_variable v
let e_application ?loc a b = make_e ?loc @@ E_application {lamb=a ; args=b}
let e_lambda ?loc binder input_type output_type result : expression = make_e ?loc @@ E_lambda {binder; input_type; output_type; result}
let e_recursive ?loc fun_name fun_type lambda = make_e ?loc @@ E_recursive {fun_name; fun_type; lambda}
let e_let_in ?loc (binder, ascr) inline rhs let_result = make_e ?loc @@ E_let_in { let_binder = (binder, ascr) ; rhs ; let_result; inline }

let e_constructor ?loc s a : expression = make_e ?loc @@ E_constructor { constructor = Constructor s; element = a}
let e_matching ?loc a b : expression = make_e ?loc @@ E_matching {matchee=a;cases=b}

let e_record_accessor ?loc a b = make_e ?loc @@ E_record_accessor {record = a; path = Label b}
let e_accessor_list ?loc a b  = List.fold_left (fun a b -> e_record_accessor ?loc a b) a b
let e_record_update ?loc record path update = make_e ?loc @@ E_record_update {record; path=Label path; update}

let e_annotation ?loc anno_expr ty = make_e ?loc @@ E_ascription {anno_expr; type_annotation = ty}

let e_tuple ?loc lst : expression = make_e ?loc @@ E_tuple lst
let e_tuple_accessor ?loc tuple path : expression = make_e ?loc @@ E_tuple_accessor {tuple; path}
let e_tuple_update ?loc tuple path update : expression = make_e ?loc @@ E_tuple_update {tuple; path; update}

let e_pair ?loc a b  : expression = e_tuple ?loc [a;b]
let e_cond ?loc condition then_clause else_clause = make_e ?loc @@ E_cond {condition;then_clause;else_clause}
let e_sequence ?loc expr1 expr2 = make_e ?loc @@ E_sequence {expr1; expr2}
let e_skip ?loc () = make_e ?loc @@ E_skip

let e_list ?loc lst : expression = make_e ?loc @@ E_list lst
let e_set ?loc lst : expression = make_e ?loc @@ E_set lst
let e_map ?loc lst : expression = make_e ?loc @@ E_map lst
let e_big_map ?loc lst : expression = make_e ?loc @@ E_big_map lst
let e_look_up ?loc x y = make_e ?loc @@ E_look_up (x , y)

let e_while ?loc condition body = make_e ?loc @@ E_while {condition; body}
let e_for ?loc binder start final increment body = make_e ?loc @@ E_for {binder;start;final;increment;body}
let e_for_each ?loc binder collection collection_type body = make_e ?loc @@ E_for_each {binder;collection;collection_type;body}

let ez_match_variant (lst : ((string * string) * 'a) list) =
  let lst = List.map (fun ((c,n),a) -> ((Constructor c, Var.of_name n), a) ) lst in
  Match_variant (lst,())
let e_matching_variant ?loc a (lst : ((string * string)* 'a) list) =
  e_matching ?loc a (ez_match_variant lst)
let e_record_ez ?loc (lst : (string * expr) list) : expression =
  let map = List.fold_left (fun m (x, y) -> LMap.add (Label x) y m) LMap.empty lst in
  make_e ?loc @@ E_record map
let e_record ?loc map =
  let lst = Map.String.to_kv_list map in
  e_record_ez ?loc lst 



let make_option_typed ?loc e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc e t


let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation

let e_typed_list ?loc lst t = e_annotation ?loc (e_list lst) (t_list t)
let e_typed_list_literal ?loc lst t =
  e_annotation ?loc (e_constant C_LIST_LITERAL lst) (t_list t)

let e_typed_map ?loc lst k v = e_annotation ?loc (e_map lst) (t_map k v)
let e_typed_big_map ?loc lst k v = e_annotation ?loc (e_big_map lst) (t_big_map k v)

let e_typed_set ?loc lst k = e_annotation ?loc (e_set lst) (t_set k)




let e_assign ?loc variable access_path expression =
  make_e ?loc @@ E_assign {variable;access_path;expression} 
let e_ez_assign ?loc variable access_path expression = 
  let variable = Var.of_name variable in
  let access_path = List.map (fun s -> Access_record s) access_path in
  e_assign ?loc variable access_path expression 

let get_e_accessor = fun t ->
  match t with
  | E_record_accessor {record; path} -> ok (record , path)
  | _ -> simple_fail "not an accessor"

let assert_e_accessor = fun t ->
  let%bind _ = get_e_accessor t in
  ok ()

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
  | E_tuple t -> ok @@ t
  | _ -> simple_fail "ast_core: get_e_tuple: not a tuple"

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) result = fun e ->
  match e.expression_content with
  | E_tuple [a;b] -> ok @@ (a,b)
  | _ -> fail @@ bad_kind "pair" e.location

let extract_list : expression -> (expression list) result = fun e ->
  match e.expression_content with
  | E_list lst -> ok lst
  | _ -> fail @@ bad_kind "list" e.location

let extract_record : expression -> (label * expression) list result = fun e ->
  match e.expression_content with
  | E_record lst -> ok @@ LMap.to_kv_list lst
  | _ -> fail @@ bad_kind "record" e.location

let extract_map : expression -> (expression * expression) list result = fun e ->
  match e.expression_content with
  | E_map lst -> ok lst
  | _ -> fail @@ bad_kind "map" e.location

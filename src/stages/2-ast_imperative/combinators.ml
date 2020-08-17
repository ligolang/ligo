open Types
module Option = Simple_utils.Option

module SMap = Map.String

let make_t ?(loc = Location.generated) type_content = {type_content; location=loc}

let t_constant ?loc constant lst  : type_expression = make_t ?loc @@ T_constant (constant, lst)

let t_bool ?loc ()        : type_expression = make_t ?loc @@ T_variable (Stage_common.Constant.t_bool)
let t_string ?loc ()      : type_expression = t_constant ?loc TC_string []
let t_bytes ?loc ()       : type_expression = t_constant ?loc TC_bytes []
let t_int ?loc ()         : type_expression = t_constant ?loc TC_int []
let t_operation ?loc ()   : type_expression = t_constant ?loc TC_operation []
let t_nat ?loc ()         : type_expression = t_constant ?loc TC_nat []
let t_tez ?loc ()         : type_expression = t_constant ?loc TC_mutez []
let t_unit ?loc ()        : type_expression = t_constant ?loc TC_unit []
let t_address ?loc ()     : type_expression = t_constant ?loc TC_address []
let t_signature ?loc ()   : type_expression = t_constant ?loc TC_signature []
let t_key ?loc ()         : type_expression = t_constant ?loc TC_key []
let t_key_hash ?loc ()    : type_expression = t_constant ?loc TC_key_hash []
let t_timestamp ?loc ()   : type_expression = t_constant ?loc TC_timestamp []
let t_option ?loc o       : type_expression = t_constant ?loc TC_option [o]
let t_list ?loc t         : type_expression = t_constant ?loc TC_list [t]
let t_variable ?loc n     : type_expression = make_t ?loc @@ T_variable n
let t_variable_ez ?loc n  : type_expression = t_variable ?loc @@ Var.of_name n
let t_wildcard ?loc ()    : type_expression = make_t ?loc @@ T_wildcard

let t_record ?loc record  : type_expression = make_t ?loc @@ T_record record
let t_record_ez ?loc lst =
  let lst = List.mapi (fun i (k, v) -> (Label k, {associated_type=v;decl_pos=i})) lst in
  let record = LMap.of_list lst in
  t_record ?loc (record:row_element label_map)

let t_tuple ?loc lst    : type_expression = make_t ?loc @@ T_tuple lst
let t_pair ?loc (a , b) : type_expression = t_tuple ?loc [a; b]

let t_sum ?loc sum : type_expression = make_t ?loc @@ T_sum sum
let t_sum_ez ?loc (lst:(string * type_expression) list) : type_expression =
  let aux (prev,i) (k, v) = (LMap.add (Label k) {associated_type=v;decl_pos=i} prev, i+1) in
  let (map,_) = List.fold_left aux (LMap.empty,0) lst in
  t_sum ?loc (map: row_element label_map)

let t_annoted ?loc ty str : type_expression = make_t ?loc @@ T_annoted (ty, str)

let t_function ?loc type1 type2  : type_expression = make_t ?loc @@ T_arrow {type1; type2}
let t_map ?loc key value                  : type_expression = t_constant ?loc TC_map [key; value]
let t_big_map ?loc key value              : type_expression = t_constant ?loc TC_big_map [key; value]
let t_set ?loc key                        : type_expression = t_constant ?loc TC_set [key]
let t_contract ?loc contract              : type_expression = t_constant ?loc TC_contract [contract]
let t_michelson_or ?loc l l_ann r r_ann   : type_expression = t_constant ?loc TC_michelson_or [t_annoted l l_ann; t_annoted r r_ann]
let t_michelson_pair ?loc l l_ann r r_ann : type_expression = t_constant ?loc TC_michelson_pair [t_annoted l l_ann; t_annoted r r_ann]
let t_michelson_pair_right_comb ?loc c    : type_expression = t_constant ?loc TC_michelson_pair_right_comb [c]
let t_michelson_pair_left_comb ?loc c     : type_expression = t_constant ?loc TC_michelson_pair_left_comb [c]
let t_michelson_or_right_comb ?loc c      : type_expression = t_constant ?loc TC_michelson_or_right_comb [c]
let t_michelson_or_left_comb ?loc c       : type_expression = t_constant ?loc TC_michelson_or_left_comb [c]

let get_t_annoted = fun te ->
  match te.type_content with
    T_annoted (te, lst) -> Some (te,lst)
  | _ -> None

let make_e ?(loc = Location.generated) expression_content =
  let location = loc in
  { expression_content; location }

let e_literal ?loc l : expression = make_e ?loc @@ E_literal l
let e_unit ?loc () : expression = make_e ?loc @@ E_literal (Literal_unit)
let e_int_z ?loc n : expression = make_e ?loc @@ E_literal (Literal_int n)
let e_int ?loc n : expression = e_int_z ?loc @@ Z.of_int n
let e_nat_z ?loc n : expression = make_e ?loc @@ E_literal (Literal_nat n)
let e_nat ?loc n : expression = e_nat_z ?loc @@ Z.of_int n
let e_timestamp_z ?loc n : expression = make_e ?loc @@ E_literal (Literal_timestamp n)
let e_timestamp ?loc n : expression = e_timestamp_z ?loc @@ Z.of_int n
let e_string ?loc s : expression = make_e ?loc @@ E_literal (Literal_string (Standard s))
let e_verbatim ?loc v : expression = make_e ?loc @@ E_literal (Literal_string (Verbatim v))
let e_address ?loc s : expression = make_e ?loc @@ E_literal (Literal_address s)
let e_mutez_z ?loc s : expression = make_e ?loc @@ E_literal (Literal_mutez s)
let e_mutez ?loc s : expression = e_mutez_z ?loc @@ Z.of_int s
let e_signature ?loc s : expression = make_e ?loc @@ E_literal (Literal_signature s)
let e_key ?loc s : expression = make_e ?loc @@ E_literal (Literal_key s)
let e_key_hash ?loc s : expression = make_e ?loc @@ E_literal (Literal_key_hash s)
let e_chain_id ?loc s : expression = make_e ?loc @@ E_literal (Literal_chain_id s)
let e'_bytes b : expression_content option =
  try
    let bytes = Hex.to_bytes (`Hex b) in
    Some (E_literal (Literal_bytes bytes))
  with _ -> None
let e_bytes_hex_ez ?loc b : expression option =
  match e'_bytes b with
  | Some e' -> Some (make_e ?loc e')
  | None -> None
let e_bytes_raw ?loc (b: bytes) : expression = make_e ?loc @@ E_literal (Literal_bytes b)
let e_bytes_hex ?loc b : expression = e_bytes_raw ?loc @@ Hex.to_bytes b
let e_bytes_string ?loc (s: string) : expression = e_bytes_hex ?loc @@ Hex.of_string s
let e_some ?loc s  : expression = make_e ?loc @@ E_constant {cons_name = C_SOME; arguments = [s]}
let e_none ?loc () : expression = make_e ?loc @@ E_constant {cons_name = C_NONE; arguments = []}
let e_string_cat ?loc sl sr : expression = make_e ?loc @@ E_constant {cons_name = C_CONCAT; arguments = [sl ; sr ]}
let e_map_add ?loc k v old  : expression = make_e ?loc @@ E_constant {cons_name = C_MAP_ADD; arguments = [k ; v ; old]}
let e_binop ?loc name a b  = make_e ?loc @@ E_constant {cons_name = name ; arguments = [a ; b]}

let e_constant ?loc name lst = make_e ?loc @@ E_constant {cons_name=name ; arguments = lst}
let e_variable ?loc v = make_e ?loc @@ E_variable v
let e_variable_ez ?loc v = e_variable ?loc @@ Location.wrap ?loc (Var.of_name v)
let e_application ?loc a b = make_e ?loc @@ E_application {lamb=a ; args=b}
let e_lambda ?loc binder result : expression = make_e ?loc @@ E_lambda {binder; result}
let e_recursive ?loc fun_name fun_type lambda = make_e ?loc @@ E_recursive {fun_name; fun_type; lambda}
(* let e_recursive_ez ?loc fun_name fun_type lambda = e_recursive ?loc (Var.of_name fun_name) fun_type lambda *)
let e_let_in ?loc let_binder inline rhs let_result = make_e ?loc @@ E_let_in { let_binder; rhs ; let_result; inline }
(* let e_let_in_ez ?loc binder ascr inline rhs let_result = e_let_in ?loc (Var.of_name binder, ascr) inline rhs let_result *)
let e_raw_code ?loc language code = make_e ?loc @@ E_raw_code {language; code}

let e_constructor ?loc s a : expression = make_e ?loc @@ E_constructor { constructor = Label s; element = a}
let e_true  ?loc (): expression = e_constructor ?loc "true"  @@ e_unit ?loc ()
let e_false ?loc (): expression = e_constructor ?loc "false" @@ e_unit ?loc ()
let e_matching ?loc a b : expression = make_e ?loc @@ E_matching {matchee=a;cases=b}

let e_accessor ?loc record path      = make_e ?loc @@ E_accessor {record; path}
let e_update ?loc record path update = make_e ?loc @@ E_update {record; path; update}

let e_annotation ?loc anno_expr ty = make_e ?loc @@ E_ascription {anno_expr; type_annotation = ty}

let e_tuple ?loc lst : expression = make_e ?loc @@ E_tuple lst

let e_pair ?loc a b  : expression = e_tuple ?loc [a;b]
let e_cond ?loc condition then_clause else_clause = make_e ?loc @@ E_cond {condition;then_clause;else_clause}
let e_sequence ?loc expr1 expr2 = make_e ?loc @@ E_sequence {expr1; expr2}
let e_skip ?loc () = make_e ?loc @@ E_skip

let e_list ?loc lst : expression = make_e ?loc @@ E_list lst
let e_set ?loc lst : expression = make_e ?loc @@ E_set lst
let e_map ?loc lst : expression = make_e ?loc @@ E_map lst
let e_big_map ?loc lst : expression = make_e ?loc @@ E_big_map lst

let e_while ?loc condition body = make_e ?loc @@ E_while {condition; body}
let e_for ?loc binder start final increment body = make_e ?loc @@ E_for {binder;start;final;increment;body}
let e_for_each ?loc binder collection collection_type body = make_e ?loc @@ E_for_each {binder;collection;collection_type;body}

(* let e_for_ez ?loc binder start final increment body = e_for ?loc (Var.of_name binder) start final increment body *)
(* let e_for_each_ez ?loc (b,bo) collection collection_type body = e_for_each ?loc (Var.of_name b, Option.map Var.of_name bo) collection collection_type body *)

let e_bool ?loc   b : expression = e_constructor ?loc (string_of_bool b) (e_unit ())

let e_matching_variant ?loc a lst = e_matching ?loc a @@ Match_variant lst
let e_matching_record   ?loc m lst expr = e_matching ?loc m @@ Match_record   (lst, expr)
let e_matching_tuple    ?loc m lst expr = e_matching ?loc m @@ Match_tuple    (lst, expr)
let e_matching_variable ?loc m var expr = e_matching ?loc m @@ Match_variable (var, expr)

(* let e_matching_tuple_ez ?loc m lst ty_opt expr =
  let lst = List.map Var.of_name lst in
  e_matching_tuple ?loc m lst ty_opt expr *)

(* let ez_match_variant (lst : ((string * string) * 'a) list) =
  let lst = List.map (fun ((c,n),a) -> ((Constructor c, Var.of_name n), a) ) lst in
  Match_variant lst *)

let e_record ?loc map = make_e ?loc @@ E_record map
let e_record_ez ?loc (lst : (string * expr) list) : expression =
  let map = List.fold_left (fun m (x, y) -> LMap.add (Label x) y m) LMap.empty lst in
  e_record ?loc map

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




let e_assign ?loc variable access_path expression = make_e ?loc @@ E_assign {variable;access_path;expression} 
let e_assign_ez ?loc variable access_path expression = e_assign ?loc (Location.wrap ?loc @@ Var.of_name variable) access_path expression


let get_e_accessor = fun t ->
  match t with
  | E_accessor {record; path} -> Some (record , path)
  | _ -> None

let assert_e_accessor = fun t ->
  match get_e_accessor t with
  | None -> None
  | Some _ -> Some ()

let get_e_pair = fun t ->
  match t with
  | E_tuple [a ; b] -> Some (a , b)
  | _ -> None

let get_e_list = fun t ->
  match t with
  | E_list lst -> Some lst
  | _ -> None

let get_e_tuple = fun t ->
  match t with
  | E_tuple t -> Some t
  | _ -> None

let get_e_lambda = fun e ->
  match e with
    E_lambda e -> Some e
  | _ -> None

let get_e_ascription = fun e ->
  match e with
    E_ascription e -> Some e
  | _ -> None

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option = fun e ->
  match e.expression_content with
  | E_tuple [a;b] -> Some (a,b)
  | _ -> None

let extract_list : expression -> expression list option = fun e ->
  match e.expression_content with
  | E_list lst -> Some lst
  | _ -> None

let extract_record : expression -> (label * expression) list option = fun e ->
  match e.expression_content with
  | E_record lst -> Some (LMap.to_kv_list lst)
  | _ -> None

let extract_map : expression -> (expression * expression) list option = fun e ->
  match e.expression_content with
  | E_map lst -> Some lst
  | _ -> None

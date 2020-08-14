open Types
module Option = Simple_utils.Option

module SMap = Map.String

let make_t ?(loc = Location.generated) ?sugar content = ({content; sugar; location=loc}: type_expression)

let tuple_to_record lst =
  let aux (i,acc) el = (i+1,(string_of_int i, el)::acc) in
  let (_, lst ) = List.fold_left aux (0,[]) lst in
  lst

let type_constant ?loc ?sugar type_constant arguments  : type_expression = make_t ?loc ?sugar @@ T_constant {type_constant; arguments}

let t_bool      ?loc ?sugar () : type_expression = make_t ?loc ?sugar @@ T_variable (Var.of_name "bool")
let t_string    ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_string []
let t_bytes     ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_bytes []
let t_int       ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_int []
let t_operation ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_operation []
let t_nat       ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_nat []
let t_tez       ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_mutez []
let t_unit      ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_unit []
let t_address   ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_address []
let t_signature ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_signature []
let t_key       ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_key []
let t_key_hash  ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_key_hash []
let t_timestamp ?loc ?sugar () : type_expression = type_constant ?loc ?sugar TC_timestamp []
let t_option    ?loc ?sugar o  : type_expression = type_constant ?loc ?sugar TC_option [o]
let t_list      ?loc ?sugar t  : type_expression = type_constant ?loc ?sugar TC_list [t]
let t_variable  ?loc ?sugar n  : type_expression = make_t ?loc ?sugar @@ T_variable (Var.of_name n)
let t_record_ez ?loc ?sugar lst =
  let lst = List.map (fun (k, v) -> (Label k, v)) lst in
  let m = LMap.of_list lst in
  make_t ?loc ?sugar @@ T_record m
let t_record ?loc ?sugar m  : type_expression =
  let lst = Map.String.to_kv_list m in
  t_record_ez ?loc ?sugar lst

let t_pair  ?loc ?sugar (a , b) : type_expression = t_record_ez ?loc ?sugar [("0",a) ; ("1",b)]
let t_tuple ?loc ?sugar lst     : type_expression = t_record_ez ?loc ?sugar (tuple_to_record lst)

let ez_t_sum ?loc ?sugar (lst:(string * row_element) list) : type_expression =
  let aux prev (k, v) = LMap.add (Label k) v prev in
  let map = List.fold_left aux LMap.empty lst in
  make_t ?loc ?sugar @@ T_sum map
let t_sum ?loc ?sugar m : type_expression =
  let lst = Map.String.to_kv_list m in
  ez_t_sum ?loc ?sugar lst

let t_function ?loc ?sugar type1 type2  : type_expression = make_t ?loc ?sugar @@ T_arrow {type1; type2}
let t_map      ?loc ?sugar key value : type_expression = type_constant ?loc ?sugar TC_map [key ; value]
let t_big_map  ?loc ?sugar key value : type_expression = type_constant ?loc ?sugar TC_big_map [key ; value]
let t_set      ?loc ?sugar t         : type_expression = type_constant ?loc ?sugar TC_set [t]
let t_contract ?loc ?sugar t         : type_expression = type_constant ?loc ?sugar TC_contract [t]

let make_e ?(loc = Location.generated) ?sugar content = {content; sugar; location=loc }

let e_var       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_variable (Location.wrap ?loc (Var.of_name n))
let e_literal   ?loc ?sugar l  : expression = make_e ?loc ?sugar @@ E_literal l
let e_unit      ?loc ?sugar () : expression = make_e ?loc ?sugar @@ E_literal (Literal_unit)
let e_int       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_int n)
let e_nat       ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_nat n)
let e_timestamp ?loc ?sugar n  : expression = make_e ?loc ?sugar @@ E_literal (Literal_timestamp n)
let e_string    ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_string s)
let e_address   ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_address s)
let e_mutez     ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_mutez s)
let e_signature ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_signature s)
let e_key       ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key s)
let e_key_hash  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_key_hash s)
let e_chain_id  ?loc ?sugar s  : expression = make_e ?loc ?sugar @@ E_literal (Literal_chain_id s)
let e'_bytes b : expression_content =
  let bytes = Hex.to_bytes (`Hex b) in
  E_literal (Literal_bytes bytes)
let e_bytes_hex ?loc ?sugar b : expression =
  let e' = e'_bytes b in
  make_e ?loc ?sugar e'
let e_bytes_raw ?loc ?sugar (b: bytes) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes b)
let e_bytes_string ?loc ?sugar (s: string) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))
let e_some       ?loc ?sugar s        : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_SOME; arguments = [s]}
let e_none       ?loc ?sugar ()       : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_NONE; arguments = []}
let e_string_cat ?loc ?sugar sl sr    : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_CONCAT; arguments = [sl ; sr ]}
let e_map_add    ?loc ?sugar k v old  : expression = make_e ?loc ?sugar @@ E_constant {cons_name = C_MAP_ADD; arguments = [k ; v ; old]}

let e_constant    ?loc ?sugar name lst                             = make_e ?loc ?sugar @@ E_constant {cons_name=name ; arguments = lst}
let e_variable    ?loc ?sugar v                                    = make_e ?loc ?sugar @@ E_variable v
let e_application ?loc ?sugar a b                                  = make_e ?loc ?sugar @@ E_application {lamb=a ; args=b}
let e_lambda      ?loc ?sugar binder input_type output_type result = make_e ?loc ?sugar @@ E_lambda {binder; input_type; output_type; result ;  }
let e_recursive   ?loc ?sugar fun_name fun_type lambda             = make_e ?loc ?sugar @@ E_recursive {fun_name; fun_type; lambda}
let e_let_in      ?loc ?sugar (binder, ascr) inline rhs let_result = make_e ?loc ?sugar @@
  E_let_in { let_binder = {binder ; ascr} ; rhs ; let_result; inline }
let e_raw_code    ?loc ?sugar language code                        = make_e ?loc ?sugar @@ E_raw_code {language; code}

let e_constructor ?loc ?sugar s a : expression = make_e ?loc ?sugar @@ E_constructor { constructor = Label s; element = a}
let e_matching    ?loc ?sugar a b : expression = make_e ?loc ?sugar @@ E_matching {matchee=a;cases=b}

let e_record          ?loc ?sugar map = make_e ?loc ?sugar @@ E_record map
let e_record_accessor ?loc ?sugar a b = make_e ?loc ?sugar @@ E_record_accessor {record = a; path = b}
let e_record_update   ?loc ?sugar record path update = make_e ?loc ?sugar @@ E_record_update {record; path; update}

let e_annotation ?loc ?sugar anno_expr ty = make_e ?loc ?sugar @@ E_ascription {anno_expr; type_annotation = ty}

let e_bool ?loc ?sugar b : expression = e_constructor ?loc ?sugar (string_of_bool b) (e_unit ())

let make_option_typed ?loc ?sugar e t_opt =
  match t_opt with
  | None -> e
  | Some t -> e_annotation ?loc ?sugar e t

let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation



let get_e_record_accessor = fun t ->
  match t with
  | E_record_accessor {record; path} -> Some (record, path)
  | _ -> None

let assert_e_record_accessor = fun t ->
  match get_e_record_accessor t with
  | Some _ -> Some ()
  | None -> None

let get_e_pair = fun t ->
  match t with
  | E_record r -> ( 
  let lst = LMap.to_kv_list r in
    match lst with 
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] -> 
        Some (a , b)
    | _ -> None
    )
  | _ -> None

let get_e_list = fun t ->
  let rec aux t = 
    match t with
      E_constant {cons_name=C_CONS;arguments=[key;lst]} -> 
        let lst = aux lst.content in
        (Some key)::(lst)
    | E_constant {cons_name=C_LIST_EMPTY;arguments=[]} ->
        []
    | _ -> [None]
  in
  let opts = aux t in
  if List.exists (Option.is_none) opts then None
  else Some (List.map Option.unopt_exn opts)

let get_e_tuple = fun t ->
  match t with
  | E_record r -> Some (List.map snd @@ Helpers.tuple_of_record r)
  | _ -> None

let get_e_ascription = fun a ->
  match a with 
  | E_ascription {anno_expr; type_annotation} -> Some (anno_expr,type_annotation)
  | _ -> None

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option = fun e ->
  match e.content with
  | E_record r -> ( 
  let lst = LMap.to_kv_list r in
    match lst with 
    | [(Label "O",a);(Label "1",b)]
    | [(Label "1",b);(Label "0",a)] -> 
      Some (a , b)
    | _ -> None
    )
  | _ -> None

let extract_record : expression -> (label * expression) list option = fun e ->
  match e.content with
  | E_record lst -> Some (LMap.to_kv_list lst)
  | _ -> None

let extract_map : expression -> (expression * expression) list option = fun e ->
  let rec aux e =
    match e.content with
      E_constant {cons_name=C_UPDATE|C_MAP_ADD; arguments=[k;v;map]} -> 
        let map = aux map in
        (Some (k,v))::map 
    | E_constant {cons_name=C_MAP_EMPTY|C_BIG_MAP_EMPTY; arguments=[]} -> []
    | _ -> [None]
  in
  let opts = aux e in
  if List.exists (Option.is_none) opts then None
  else Some (List.map Option.unopt_exn opts)

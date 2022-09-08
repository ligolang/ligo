module Location    = Simple_utils.Location
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module S = Ast_core
open Types

(* Helpers for accessing and constructing elements are derived using
   `ppx_woo` (`@@deriving ez`) *)

type expression_content = [%import: Types.expression_content]
[@@deriving ez {
      prefixes = [
        ("make_e" , fun ?(location = Location.generated) expression_content type_expression ->
                  ({ expression_content ; location ; type_expression } : expression)) ;
        ("get" , fun x -> x.expression_content) ;
        ("get_type" , fun x -> x.type_expression) ;
      ] ;
      wrap_constructor = ("expression_content" , make_e) ;
      wrap_get = ("expression_content" , get) ;
    } ]

type type_content = [%import: Types.type_content]
[@@deriving ez {
      prefixes = [
        ("make_t" , fun  ?(loc = Location.generated) type_content type_meta ->
                  ({ type_content ; location = loc ; orig_var = None ; type_meta } : type_expression)) ;
        ("get" , fun x -> x.type_content) ;
      ] ;
      wrap_constructor = ("type_content" , (fun type_content ?loc ?type_meta () -> make_t ?loc type_content type_meta)) ;
      wrap_get = ("type_content" , get) ;
      default_get = `Option ;
    } ]

open Ligo_prim
open Literal_types

let make_t_orig_var ?(loc = Location.generated) type_content core orig_var = {type_content; location=loc; type_meta = core ; orig_var}

let t_constant ?loc ?core injection parameters : type_expression =
  make_t ?loc (T_constant {language=Backend.Michelson.name; injection = injection ; parameters}) core

(* TODO?: X_name here should be replaced by X_injection *)
let t__type_ ?loc ?core () : type_expression = t_constant ?loc ?core _type_ []
[@@map (_type_, (
    "signature","chain_id", "string", "bytes", "key", "key_hash", "int", "address", "operation", "nat", "tez",
    "timestamp", "unit", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr", "never", "mutation", "pvss_key", "baker_hash",
    "chest_key", "chest" , "tx_rollup_l2_address", "michelson_contract", "ast_contract"
  ))]

let t__type_ ?loc ?core t : type_expression = t_constant ?loc ?core _type_ [t]
[@@map (_type_, ("list", "set", "contract", "ticket", "sapling_state", "sapling_transaction", "gen"))]

let t_ext_int ?loc ?core t : type_expression = t_constant ?loc ?core (External "int") [t]
let t_ext_ediv ?loc ?core t t' : type_expression = t_constant ?loc ?core (External "ediv") [t; t']
let t_ext_u_ediv ?loc ?core t t' : type_expression = t_constant ?loc ?core (External "u_ediv") [t; t']

let t__type_ ?loc ?core t t' : type_expression = t_constant ?loc ?core _type_ [t; t']
[@@map (_type_, ("map", "big_map", "typed_address"))]

let t_mutez = t_tez

let t_abstraction1 ?loc name kind : type_expression =
  let ty_binder = Type_var.fresh ~name:"_a" () in
  let type_ = t_constant name [t_variable ~type_meta:(Ast_core.t_variable ty_binder ()) ty_binder ()] in
  t_abstraction { ty_binder ; kind ; type_ } ?loc ()
let t_abstraction2 ?loc name kind_l kind_r : type_expression =
  let ty_binder_l = Type_var.fresh ~name:"_l" () in
  let ty_binder_r = Type_var.fresh ~name:"_r" () in
  let type_ = t_constant name
    [ t_variable ty_binder_l () ;
      t_variable ty_binder_r () ]
  in
  t_abstraction ?loc { ty_binder = ty_binder_l ; kind = kind_l ; type_ = t_abstraction ?loc { ty_binder = ty_binder_r ; kind = kind_r ; type_ } () } ()
let t_for_all ty_binder kind type_ = t_for_all { ty_binder ; kind ; type_ } ()

let t_record ?loc ?core ~layout fields  : type_expression = t_record ?loc { fields ; layout } ?type_meta:core ()
let default_layout = Layout.L_tree
let make_t_ez_record ?loc ?core ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label.of_string x, ({associated_type=y;michelson_annotation=None;decl_pos=i} : row_element)) ) lst in
  let map = Record.of_list lst in
  t_record ?loc ?core ~layout map

let ez_t_record ?loc ?core ?(layout=default_layout) lst : type_expression =
  let m = Record.of_list lst in
  t_record ?loc ?core ~layout m
let t_pair ?loc ?core a b : type_expression =
  ez_t_record ?loc ?core [
    (Label.of_int 0,Rows.{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label.of_int 1,Rows.{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ]

let t_triplet ?loc ?core a b c : type_expression =
  ez_t_record ?loc ?core [
    (Label.of_int 0,Rows.{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label.of_int 1,Rows.{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ;
    (Label.of_int 2,Rows.{associated_type=c;michelson_annotation=None ; decl_pos = 2}) ]

let t_tuple ?loc ?core xs : type_expression =
  ez_t_record ?loc ?core @@
    List.mapi ~f:(fun i t -> (Label.of_int i, Rows.{associated_type=t;michelson_annotation=None ; decl_pos = i})) xs

let t_sum ?loc ?core ~layout fields : type_expression = t_sum ?loc { fields ; layout } ?type_meta:core ()
let t_sum_ez ?loc ?core ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label.of_string x, ({associated_type=y;michelson_annotation=None;decl_pos=i}:row_element)) ) lst in
  let map = Record.of_list lst in
  t_sum ?loc ?core ~layout map
let t_record_ez ?loc ?core ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label.of_string x, ({associated_type=y;michelson_annotation=None;decl_pos=i}:row_element)) ) lst in
  let map = Record.of_list lst in
  t_record ?loc ?core ~layout map
let t_bool ?loc ?core ()       : type_expression = t_sum_ez ?loc ?core
  [("True", t_unit ());("False", t_unit ())]

let t_option ?loc ?core typ : type_expression =
  t_sum_ez ?loc ?core [
    ("Some", typ) ;
    ("None", t_unit ());
  ]

let t_option_abst ?loc ?core () : type_expression =
  let ty_binder = Type_var.of_input_var "'a" in
  t_abstraction { ty_binder ; kind=Type ; type_= (
    t_sum_ez ?loc ?core [
      ("Some", t_variable ty_binder ()) ;
      ("None", t_unit ());
    ])
  } ()

(* types specific to LIGO test framework*)
let t_michelson_code ?loc ?core () : type_expression = t_constant ?loc ?core Literal_types.Michelson_program []
let t_test_exec_error ?loc ?core () : type_expression = t_sum_ez ?loc ?core [
    "Rejected", t_pair (t_michelson_code ()) (t_address ()) ;
    "Balance_too_low", t_record_ez [
        "contract_too_low" , t_address () ;
        "contract_balance" , t_mutez () ;
        "spend_request" , t_mutez () ;
      ];
    "Other" , t_string ()
  ]
let t_test_exec_result ?loc ?core () : type_expression = t_sum_ez ?loc ?core
  [ ("Success" ,t_nat ()); ("Fail", t_test_exec_error ())]
let t_test_baker_policy ?loc ?core () : type_expression = t_sum_ez ?loc ?core
  [ ("By_round" ,t_int ()); ("By_account", t_address ()); ("Excluding", t_list (t_address ()))]

let t_arrow param result ?loc ?s () : type_expression = t_arrow ?loc ?type_meta:s {type1=param; type2=result} ()
let t_shallow_closure param result ?loc ?s () : type_expression = make_t ?loc (T_arrow {type1=param; type2=result}) s
let t_chest_opening_result ?loc ?core () : type_expression =
  t_sum_ez ?loc ?core [
    ("Ok_opening", t_bytes ()) ;
    ("Fail_decrypt", t_unit ());
    ("Fail_timelock", t_unit ())
  ]

let get_lambda_with_type e =
  match (e.expression_content , e.type_expression.type_content) with
  | E_lambda l , T_arrow {type1;type2} -> Some (l , (type1,type2))
  | _ -> None

let get_t_bool (t:type_expression) : unit option = match t.type_content with
  | t when (compare_type_content t (t_bool ()).type_content) = 0 -> Some ()
  | _ -> None

let get_t_option (t:type_expression) : type_expression option =
  let l_none = Label.of_string "None" in
  let l_some = Label.of_string "Some" in
  match t.type_content with
  | T_sum {fields;_} ->
    let keys = Record.LMap.keys fields in
    (match keys with
      [a ; b] when (Label.equal a l_none && Label.equal b l_some)
    || (Label.equal a l_some && Label.equal b l_none) ->
      let some = Record.LMap.find l_some fields in
      Some some.Rows.associated_type
    | _ -> None)
  | _ -> None

let get_param_inj (t:type_expression) : (string * Literal_types.t * type_expression list) option =
  match t.type_content with
  | T_constant {language;injection;parameters} -> Some (language,injection,parameters)
  | _ -> None

let get_t_inj (t:type_expression) (v:Literal_types.t) : (type_expression list) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters} when Literal_types.equal injection v -> Some parameters
  | _ -> None
let get_t_base_inj (t:type_expression) (v:Literal_types.t) : unit option =
  match get_t_inj t v with
  | Some [] -> Some ()
  | _ -> None
let get_t_unary_inj (t:type_expression) (v:Literal_types.t) : type_expression option =
  match get_t_inj t v with
  | Some [a] -> Some a
  | _ -> None
let get_t_binary_inj (t:type_expression) (v:Literal_types.t) : (type_expression * type_expression) option =
  match get_t_inj t v with
  | Some [a;b] -> Some (a,b)
  | _ -> None

let get_t__type_ (t : type_expression) : unit option = get_t_base_inj t _type_
[@@map (_type_, ("int", "nat", "unit", "tez", "timestamp", "address", "bytes", "string", "key", "signature", "key_hash", "chest", "chest_key", "michelson_program", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr"))]

let get_t__type_ (t : type_expression) : type_expression option = get_t_unary_inj t _type_
[@@map (_type_, ("contract",  "list", "set", "ticket", "sapling_state", "sapling_transaction"))]

let get_t_mutez (t:type_expression) : unit option = get_t_tez t
let get_t_michelson_code (t:type_expression) : unit option = get_t_michelson_program t

let tuple_of_record (m: _ Record.t) =
  let aux i =
    let opt = Record.LMap.find_opt (Label.of_int i) m in
    Option.bind ~f: (fun opt -> Some (opt,i+1)) opt
  in
  let l = Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux in
  List.map ~f:(fun ({associated_type;_} : row_element) -> associated_type) l


let get_t_tuple (t:type_expression) : type_expression list option = match t.type_content with
  | T_record record -> Some (tuple_of_record record.fields)
  | _ -> None

let get_t_pair (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_record m ->
      let lst = tuple_of_record m.fields in
      ( match lst with
        | [fst;snd] -> Some (fst,snd)
        | _ -> None
      )
  | _ -> None

let get_t_or (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_sum m ->
      let lst = List.map ~f:(fun (a,({associated_type;_}:row_element)) -> a,associated_type) @@ Record.LMap.to_kv_list m.fields in
      ( match lst with
        | [(Label "left",l);(Label "right",r)]
        | [(Label "right",r);(Label "left",l)] -> Some (l,r)
        | _ -> None
      )
  | _ -> None
let get_t_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when Literal_types.equal injection Literal_types.Map -> Some (k,v)
  | _ -> None

let get_t_typed_address (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when Literal_types.equal injection Literal_types.Typed_address -> Some (k,v)
  | _ -> None

let get_t_big_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when Literal_types.equal injection Literal_types.Big_map -> Some (k,v)
  | _ -> None

let get_t__type__exn t = match get_t__type_ t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))
[@@map (_type_, ("list", "set", "map", "typed_address", "big_map"))]

let assert_t_contract (t:type_expression) : unit option = match get_t_unary_inj t Literal_types.Contract with
  | Some _ -> Some ()
  | _ -> None

let is_t__type_ t = Option.is_some (get_t__type_ t)
[@@map (_type_, ("list", "set", "nat", "string", "bytes", "int", "unit", "address", "tez", "contract", "map", "big_map"))]

let is_t_mutez t = is_t_tez t

let assert_t_list_operation (t : type_expression) : unit option =
  match get_t_list t with
  | Some t' -> get_t_base_inj t' Literal_types.Operation
  | None -> None

let assert_t__type_ : type_expression -> unit option = fun t -> get_t__type_ t
[@@map (_type_, ("int", "nat", "unit", "mutez", "key", "signature", "key_hash", "bytes", "string", "michelson_code"))]

let assert_t__type_ : type_expression -> unit option = fun v -> Option.map ~f:(fun _ -> ()) @@ get_t__type_ v
[@@map (_type_, ("set", "list"))]

let ez_e_record (lst : (Label.t * expression) list) : expression_content =
  let aux prev (k, v) = Record.LMap.add k v prev in
  let map = List.fold_left ~f:aux ~init:Record.LMap.empty lst in
  E_record map

let e__ct_ () : expression_content = E_constant { cons_name = C__CT_; arguments = [] }
[@@map (_ct_, ("none", "nil", "set_empty", "map_empty", "big_map_empty"))]

let e__ct_ p : expression_content = E_constant { cons_name = C__CT_; arguments = [p] }
[@@map (_ct_, ("some", "contract_opt", "contract"))]

let e__ct_ p p' : expression_content = E_constant { cons_name = C__CT_; arguments = [p; p']}
[@@map (_ct_, ("cons", "set_add", "map_remove", "contract_entrypoint", "contract_entrypoint_opt"))]

let e_map_add k v tl : expression_content = E_constant {cons_name=C_MAP_ADD;arguments=[k;v;tl]}
let e_unpack e : expression_content = E_constant {cons_name=C_BYTES_UNPACK; arguments=[e]}

let e__type_ p : expression_content = E_literal (Literal__type_ p)
[@@map (_type_, ("int", "nat", "mutez", "string", "bytes", "timestamp", "address", "signature", "key", "key_hash", "chain_id", "operation", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr"))]
let e_unit () : expression_content = E_literal (Literal_unit)

let e_pair a b : expression_content = ez_e_record [(Label.of_int 0,a);(Label.of_int 1, b)]

let e_bool b : expression_content =
  if b then
    E_constructor { constructor = (Label "True") ; element = (make_e (e_unit ()) (t_unit())) }
  else
    E_constructor { constructor = (Label "False") ; element = (make_e (e_unit ()) (t_unit())) }

let e_a_literal l t = make_e (E_literal l) t
let e_a__type_ p = make_e (e__type_ p) (t__type_ ())
[@@map (_type_, ("unit", "int", "nat", "mutez", "timestamp", "key_hash", "string", "bytes", "address", "key", "signature", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr"))]

let e_a_pair a b = make_e (e_pair a b)
  (t_pair a.type_expression b.type_expression )
let e_a_constructor constructor element t = e_constructor { constructor = (Label constructor) ; element } t

let e_a_record ?(layout=default_layout) r = e_record r (t_record ~layout
  (Record.map
    (fun t ->
      let associated_type = get_type t in
      Rows.{associated_type ; michelson_annotation=None ; decl_pos = 0} )
    r ))
let ez_e_a_record ?layout r = make_e (ez_e_record r) (ez_t_record ?layout (List.mapi ~f:(fun i (x, y) -> x, Rows.{associated_type = y.type_expression ; michelson_annotation = None ; decl_pos = i}) r))
let e_a_variable v ty = e_variable v ty
let e_a_application lamb args t = e_application {lamb;args} t
let e_a_lambda l in_ty out_ty = e_lambda l (t_arrow in_ty out_ty ())
let e_a_recursive l= e_recursive l l.fun_type
let e_a_let_in let_binder rhs let_result attr = e_let_in { let_binder ; rhs ; let_result ; attr } (get_type let_result)
let e_a_raw_code language code t = e_raw_code { language ; code } t
let e_a_type_inst forall type_ u = e_type_inst { forall ; type_ } u
let e_a_mod_in module_binder rhs let_result = e_mod_in { module_binder ; rhs ; let_result } (get_type let_result)

(* Constants *)
let e_a_nil t = make_e (e_nil ()) (t_list t)
let e_a_cons hd tl = make_e (e_cons hd tl) (t_list hd.type_expression)
let e_a_set_empty t = make_e (e_set_empty ()) (t_set t)
let e_a_set_add hd tl = make_e (e_set_add hd tl) (t_set hd.type_expression)
let e_a_map_empty kt vt = make_e (e_map_empty ()) (t_map kt vt)
let e_a_map_add k v tl = make_e (e_map_add k v tl) (t_map k.type_expression v.type_expression)
let e_a_big_map_empty kt vt = make_e (e_big_map_empty ()) (t_big_map kt vt)
let e_a_big_map_add k v tl = make_e (e_map_add k v tl) (t_big_map k.type_expression v.type_expression)
let e_a_big_map_remove k tl = make_e (e_map_remove k tl) tl.type_expression
let e_a_contract_opt a t = make_e (e_contract_opt a) (t_option (t_contract t))
let e_a_contract a t = make_e (e_contract a) (t_contract t)
let e_a_contract_entrypoint e a t = make_e (e_contract_entrypoint e a) (t_contract t)
let e_a_contract_entrypoint_opt e a t = make_e (e_contract_entrypoint_opt e a) (t_option (t_contract t))
let e_a_unpack e t = make_e (e_unpack e) (t_option t)

let get_a_int (t:expression) =
  match t.expression_content with
  | E_literal (Literal_int n) -> Some n
  | _ -> None

let get_a_string (t:expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None

let get_a_verbatim (t:expression) =
  match t.expression_content with
    E_literal (Literal_string (Verbatim v)) -> Some v
  | _ -> None

let get_a_unit (t:expression) =
  match t.expression_content with
  | E_literal (Literal_unit) -> Some ()
  | _ -> None

let get_record_field_type (t : type_expression) (label : Label.t) : type_expression option =
  match get_t_record t with
  | None -> None
  | Some record ->
    match Record.LMap.find_opt label record.fields with
    | None -> None
    | Some row_element -> Some row_element.Rows.associated_type

let get_record_fields (t : type_expression) : (Label.t * type_expression) list option =
  match get_t_record t with
  | None -> None
  | Some record ->
    let lst = Record.LMap.to_kv_list record.fields in
    Some (List.map ~f:(fun (k,x) -> k,x.Rows.associated_type) lst)

let get_sum_label_type (t : type_expression) (label : Label.t) : type_expression option =
  match get_t_sum t with
  | None -> None
  | Some s ->
    match Record.LMap.find_opt label s.fields with
    | None -> None
    | Some row_element -> Some row_element.Rows.associated_type

(* getter for a function of the form p * s -> ret *)
let get_view_form ty =
  match get_t_arrow ty with
  | Some { type1 = tin ; type2  = return } -> (
    match get_t_tuple tin with
    | Some [ arg ; storage ] -> Some (arg , storage , return)
    | _ -> None
  )
  | None -> None

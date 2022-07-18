module Location    = Simple_utils.Location
module List        = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Types
open Stage_common.Constant

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
        ("make_t" , fun ?(loc = Location.generated) ?source_type type_content ->
                  ({ type_content ; location = loc ; orig_var = None ; source_type } : type_expression)) ;
        ("get" , fun x -> x.type_content) ;
      ] ;
      wrap_constructor = ("type_content" , (fun type_content ?loc ?source_type () -> make_t ?loc ?source_type type_content)) ;
      wrap_get = ("type_content" , get) ;
      default_get = `Option ;
    } ]

let t_constant ?loc injection parameters : type_expression =
  t_constant ?loc {language=Stage_common.Backends.michelson; injection  ; parameters} ()

(* TODO?: X_name here should be replaced by X_injection *)
let t__type_ ?loc () : type_expression = t_constant ?loc _type_ []
[@@map (_type_, ("signature","chain_id", "string", "bytes", "key", "key_hash", "int", "address", "operation", "nat", "tez", "timestamp", "unit", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr", "never", "mutation", "pvss_key", "baker_hash", "chest_key", "chest"))]

let t__type_ ?loc t : type_expression = t_constant ?loc _type_ [t]
[@@map (_type_, ("list", "set", "contract", "ticket"))]

let t__type_ ?loc t t' : type_expression = t_constant ?loc _type_ [t; t']
[@@map (_type_, ("map", "big_map", "typed_address"))]

let t_mutez = t_tez

let t_record ?loc ~layout content  : type_expression = make_t ?loc (T_record {content;layout})
let default_layout = L_tree
let make_t_ez_record ?loc ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label x, ({associated_type=y;michelson_annotation=None;decl_pos=i} : row_element)) ) lst in
  let map = LMap.of_list lst in
  t_record ?loc ~layout map

let ez_t_record ?loc ?(layout=default_layout) lst : type_expression =
  let m = LMap.of_list lst in
  t_record ?loc ~layout m
let t_pair ?loc a b : type_expression =
  ez_t_record ?loc [
    (Label "0",{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label "1",{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ]

let t_triplet ?loc a b c : type_expression =
  ez_t_record ?loc [
    (Label "0",{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label "1",{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ;
    (Label "2",{associated_type=c;michelson_annotation=None ; decl_pos = 2}) ]
let t_human_ticket ?loc ty : type_expression =
  ez_t_record ?loc [
    (Label "ticketer", {associated_type=(t_address ()); michelson_annotation=None ; decl_pos = 0}) ;
    (Label "value"   , {associated_type=ty            ; michelson_annotation=None ; decl_pos = 1}) ;
    (Label "amount"  , {associated_type=(t_nat ())    ; michelson_annotation=None ; decl_pos = 2}) ]

let t_sum ?loc ~layout content : type_expression = make_t ?loc (T_sum {content;layout})
let t_sum_ez ?loc ?(layout=default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label x, ({associated_type=y;michelson_annotation=None;decl_pos=i}:row_element)) ) lst in
  let map = LMap.of_list lst in
  t_sum ?loc ~layout map
let t_bool ?loc ()       : type_expression = t_sum_ez ?loc
  [("True", t_unit ());("False", t_unit ())]

let t_option ?loc typ : type_expression =
  t_sum_ez ?loc [
    ("Some", typ) ;
    ("None", t_unit ());
  ]

(* types specific to LIGO test framework*)
let t_michelson_code ?loc () : type_expression = t_constant ?loc Stage_common.Constant.Michelson_program []
let t_test_exec_error ?loc () : type_expression = t_sum_ez ?loc
  [ ("Rejected", t_pair (t_michelson_code ()) (t_address ())) ; ("Other" , t_unit ())]
let t_test_exec_result ?loc () : type_expression = t_sum_ez ?loc
  [ ("Success" ,t_unit ()); ("Fail", t_sum_ez [ ("Rejected", t_pair (t_michelson_code ()) (t_address ())) ; ("Other" , t_unit ())])]

let t_arrow param result ?loc ?source_type () : type_expression = t_arrow ?loc ?source_type {type1=param; type2=result} ()
let t_shallow_closure param result ?loc () : type_expression = make_t ?loc (T_arrow {type1=param; type2=result})
let t_chest_opening_result ?loc () : type_expression =
  t_sum_ez ?loc [
    ("Ok_opening", t_bytes ()) ;
    ("Fail_decrypt", t_unit ());
    ("Fail_timelock", t_unit ())
  ]

let get_t_bool (t:type_expression) : unit option = match t.type_content with
  | t when (Compare.type_content t (t_bool ()).type_content) = 0 -> Some ()
  | _ -> None

let get_t_option (t:type_expression) : type_expression option =
  match t.type_content with
  | T_sum {content;_} ->
    let keys = LMap.keys content in
    (match keys with
      [Label "Some" ; Label "None"]
    | [Label "None" ; Label "Some"] ->
        let some = LMap.find (Label "Some") content in
        Some some.associated_type
    | _ -> None)
  | _ -> None

let get_param_inj (t:type_expression) : (string * Stage_common.Constant.t * type_expression list) option =
  match t.type_content with
  | T_constant {language;injection;parameters} -> Some (language,injection,parameters)
  | _ -> None

let get_t_inj (t:type_expression) (v:Stage_common.Constant.t) : (type_expression list) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters} when Stage_common.Constant.equal injection v -> Some parameters
  | _ -> None

let get_t_base_inj (t:type_expression) (v:Stage_common.Constant.t) : unit option =
  match get_t_inj t v with
  | Some [] -> Some ()
  | _ -> None

let get_t_unary_inj (t:type_expression) (v:Stage_common.Constant.t) : type_expression option =
  match get_t_inj t v with
  | Some [a] -> Some a
  | _ -> None

let get_t_binary_inj (t:type_expression) (v:Stage_common.Constant.t) : (type_expression * type_expression) option =
  match get_t_inj t v with
  | Some [a;b] -> Some (a,b)
  | _ -> None

let get_t__type_ (t : type_expression) : unit option = get_t_base_inj t _type_
[@@map (_type_, ("int", "nat", "unit", "tez", "timestamp", "address", "bytes", "string", "key", "signature", "key_hash", "chest", "chest_key", "michelson_program", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr"))]

let get_t__type_ (t : type_expression) : type_expression option = get_t_unary_inj t _type_
[@@map (_type_, ("contract", "list", "set", "ticket", "sapling_state", "sapling_transaction", "gen"))]

let get_t_mutez (t:type_expression) : unit option = get_t_tez t
let get_t_michelson_code (t:type_expression) : unit option = get_t_michelson_program t

let tuple_of_record (m: _ LMap.t) =
  let aux i =
    let opt = LMap.find_opt (Label (string_of_int i)) m in
    Option.bind ~f: (fun opt -> Some (opt,i+1)) opt
  in
  let l = Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux in
  List.map ~f:(fun {associated_type;_} -> associated_type) l

let get_t_tuple (t:type_expression) : type_expression list option = match t.type_content with
  | T_record record -> Some (tuple_of_record record.content)
  | _ -> None

let get_t_pair (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_record m ->
      let lst = tuple_of_record m.content in
      ( match List.(length lst = 2) with
        | true -> Some (List.(nth_exn lst 0 , nth_exn lst 1))
        | false -> None
      )
  | _ -> None

let get_t_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when Stage_common.Constant.equal injection Stage_common.Constant.Map -> Some (k,v)
  | _ -> None

let get_t_typed_address (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when Stage_common.Constant.equal injection Stage_common.Constant.Typed_address -> Some (k,v)
  | _ -> None

let get_t_big_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when Stage_common.Constant.equal injection Stage_common.Constant.Big_map -> Some (k,v)
  | _ -> None

let get_t__type__exn t = match get_t__type_ t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))
[@@map (_type_, ("list", "set", "map", "typed_address", "big_map", "gen"))]

let assert_t_contract (t:type_expression) : unit option = match get_t_unary_inj t Stage_common.Constant.Contract with
  | Some _ -> Some ()
  | _ -> None

let is_t__type_ t = Option.is_some (get_t__type_ t)
[@@map (_type_, ("list", "set", "nat", "string", "bytes", "int", "bool", "unit", "address", "tez", "contract", "map", "big_map"))]

let is_t_mutez t = is_t_tez t

let assert_t_list_operation (t : type_expression) : unit option =
  match get_t_list t with
  | Some t' -> get_t_base_inj t' Stage_common.Constant.Operation
  | None -> None

let assert_t__type_ : type_expression -> unit option = fun t -> get_t__type_ t
[@@map (_type_, ("int", "nat", "bool", "unit", "mutez", "key", "signature", "key_hash", "bytes", "string", "michelson_code"))]

let assert_t__type_ : type_expression -> unit option = fun v -> Option.map ~f:(fun _ -> ()) @@ get_t__type_ v
[@@map (_type_, ("set", "list"))]

let ez_e_record (lst : (label * expression) list) : expression_content =
  let aux prev (k, v) = LMap.add k v prev in
  let map = List.fold_left ~f:aux ~init:LMap.empty lst in
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
[@@map (_type_, ("int", "nat", "mutez", "string", "bytes", "timestamp", "address", "signature", "key", "key_hash", "chain_id", "operation", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr" , "chest", "chest_key"))]
let e_unit () : expression_content = E_literal (Literal_unit)

let e_pair a b : expression_content = ez_e_record [(Label "0",a);(Label "1", b)]

let e_bool b : expression_content =
  if b then
    E_constructor { constructor = (Label "True") ; element = (make_e (e_unit ()) (t_unit ())) }
  else
    E_constructor { constructor = (Label "False") ; element = (make_e (e_unit ()) (t_unit ())) }

let e_a_literal l t = make_e (E_literal l) t
let e_a__type_ p = make_e (e__type_ p) (t__type_ ())
[@@map (_type_, ("unit", "int", "nat", "mutez", "timestamp", "key_hash", "bool", "string", "bytes", "address", "key", "signature", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr" , "chest", "chest_key"))]

let e_a_pair a b = make_e (e_pair a b)
  (t_pair a.type_expression b.type_expression )
let e_a_constructor constructor element t = e_constructor { constructor = (Label constructor) ; element } t
let e_a_record_accessor record path t = e_record_accessor {record ; path} t
let e_a_record ?(layout=default_layout) r = e_record r (t_record ~layout
  (LMap.map
    (fun t ->
      let associated_type = get_type t in
      {associated_type ; michelson_annotation=None ; decl_pos = 0} )
    r ))
let ez_e_a_record ?layout r = make_e (ez_e_record r) (ez_t_record ?layout (List.mapi ~f:(fun i (x, y) -> x, {associated_type = y.type_expression ; michelson_annotation = None ; decl_pos = i}) r))
let e_a_variable v ty = e_variable v ty
let e_a_application lamb args t = e_application {lamb;args} t
let e_a_lambda l in_ty out_ty = e_lambda l (t_arrow in_ty out_ty ())
let e_a_recursive l= e_recursive l l.fun_type
let e_a_let_in let_binder rhs let_result attr = e_let_in { let_binder ; rhs ; let_result ; attr } (get_type let_result)
let e_a_raw_code language code t = e_raw_code { language ; code } t
let e_a_type_inst forall type_ u = e_type_inst { forall ; type_ } u

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

let get_a_bool (t:expression) =
  match t.expression_content with
  | E_constructor {constructor=Label name;element} when
    (String.equal name "True")
    && Compare.expression_content element.expression_content @@ e_unit () = 0 ->
      Some true
  | E_constructor {constructor=Label name;element} when
    (String.equal name "False")
    && Compare.expression_content element.expression_content @@ e_unit () = 0 ->
      Some false
  | _ -> None

let get_record_field_type (t : type_expression) (label : label) : type_expression option =
  match get_t_record_opt t with
  | None -> None
  | Some record ->
    match LMap.find_opt label record.content with
    | None -> None
    | Some row_element -> Some row_element.associated_type

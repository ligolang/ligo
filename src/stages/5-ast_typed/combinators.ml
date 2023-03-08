module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module S = Ast_core
open Ligo_prim
open Literal_types
open Types

(* Helpers for accessing and constructing elements are derived using
   `ppx_woo` (`@@deriving ez`) *)

type expression_content = [%import: Types.expression_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_e"
          , fun ~loc expression_content type_expression : expression ->
              { expression_content; location = loc; type_expression } )
        ; ("get", fun x -> x.expression_content)
        ; ("get_type", fun x -> x.type_expression)
        ]
    ; wrap_constructor =
        ("expression_content", fun content type_ ~loc -> make_e ~loc content type_)
    ; wrap_get = "expression_content", get
    }]

type type_content = [%import: Types.type_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ~loc type_content type_meta : type_expression ->
              { type_content; location = loc; orig_var = None; type_meta } )
        ; ("get", fun x -> x.type_content)
        ]
    ; wrap_constructor =
        ( "type_content"
        , fun type_content ~loc ?type_meta () -> make_t ~loc type_content type_meta )
    ; wrap_get = "type_content", get
    ; default_get = `Option
    }]

let make_t_orig_var ~loc type_content core orig_var =
  { type_content; location = loc; type_meta = core; orig_var }


let t_constant ~loc ?core injection parameters : type_expression =
  make_t
    ~loc
    (T_constant { language = Backend.Michelson.name; injection; parameters })
    core


(* TODO?: X_name here should be replaced by X_injection *)
let t__type_ ~loc ?core () : type_expression = t_constant ~loc ?core _type_ []
  [@@map
    _type_
    , ( "signature"
      , "chain_id"
      , "string"
      , "bytes"
      , "key"
      , "key_hash"
      , "int"
      , "address"
      , "operation"
      , "nat"
      , "tez"
      , "timestamp"
      , "unit"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr"
      , "never"
      , "mutation"
      , "pvss_key"
      , "baker_hash"
      , "chest_key"
      , "chest"
      , "tx_rollup_l2_address"
      , "michelson_contract"
      , "ast_contract"
      , "int64" )]


let t__type_ ~loc ?core t : type_expression = t_constant ~loc ?core _type_ [ t ]
  [@@map
    _type_
    , ( "list"
      , "set"
      , "contract"
      , "ticket"
      , "sapling_state"
      , "sapling_transaction"
      , "gen"
      , "views" )]


let t_ext_int ~loc ?core t : type_expression =
  t_constant ~loc ?core (External "int") [ t ]


let t_ext_ediv ~loc ?core t t' : type_expression =
  t_constant ~loc ?core (External "ediv") [ t; t' ]


let t_ext_u_ediv ~loc ?core t t' : type_expression =
  t_constant ~loc ?core (External "u_ediv") [ t; t' ]


let t__type_ ~loc ?core t t' : type_expression = t_constant ~loc ?core _type_ [ t; t' ]
  [@@map _type_, ("map", "big_map", "typed_address")]


let t_mutez = t_tez

let t_abstraction1 ~loc name kind : type_expression =
  let ty_binder = Type_var.fresh ~loc ~name:"_a" () in
  let type_ =
    t_constant
      ~loc
      name
      [ t_variable ~loc ~type_meta:(Ast_core.t_variable ~loc ty_binder ()) ty_binder () ]
  in
  t_abstraction { ty_binder; kind; type_ } ~loc ()


let t_abstraction2 ~loc name kind_l kind_r : type_expression =
  let ty_binder_l = Type_var.fresh ~loc ~name:"_l" () in
  let ty_binder_r = Type_var.fresh ~loc ~name:"_r" () in
  let type_ =
    t_constant
      ~loc
      name
      [ t_variable ~loc ty_binder_l (); t_variable ~loc ty_binder_r () ]
  in
  t_abstraction
    ~loc
    { ty_binder = ty_binder_l
    ; kind = kind_l
    ; type_ = t_abstraction ~loc { ty_binder = ty_binder_r; kind = kind_r; type_ } ()
    }
    ()


let t_for_all ty_binder kind type_ = t_for_all { ty_binder; kind; type_ } ()

let t_record ~loc ?core ~layout fields : type_expression =
  t_record ~loc ?type_meta:core (Row.create ~layout fields) ()


let default_layout = Layout.default

let fields_with_no_annot fields =
  List.map ~f:(fun (name, _) -> Layout.{ name; annot = None }) fields


(* wait, in fact the default_layout should have uncapitalized label as an annot isn't it ?*)
(* no, annots are strings (for now, cba refactoring that rn) *)

(* by default { a : int ; b : string} will be 'pair (int %a) (b %b)' I think *)
(* leave this to the backend. Typer shouldn't determine default rendered michelson *)
(* i am not so sure, or we forget the annot altogether. Oh I see, typer needs to care about annots. .Uh this is a mess. 
   Currently the typer doesn't insert any annotations. Lets keep that for now and raise an issue later 
hopefully test won't pass but I am uncertain :D *)

let ez_t_record
    ~loc
    ?core
    ?(layout = default_layout)
    (lst : (Label.t * type_expression) list)
    : type_expression
  =
  let layout = layout @@ fields_with_no_annot lst in
  let row = Row.of_alist_exn ~layout lst in
  make_t ~loc (T_record row) core


let make_t_ez_record
    ~loc
    ?core
    ?(layout = default_layout)
    (lst : (string * type_expression) list)
    : type_expression
  =
  let fields = List.map ~f:(fun (name, type_) -> Label.of_string name, type_) lst in
  ez_t_record ~loc ?core ~layout fields


let t_pair ~loc ?core a b : type_expression =
  ez_t_record ~loc ?core [ Label.of_int 0, a; Label.of_int 1, b ]


let t_triplet ~loc ?core a b c : type_expression =
  ez_t_record ~loc ?core [ Label.of_int 0, a; Label.of_int 1, b; Label.of_int 2, c ]


let t_tuple ~loc ?core xs : type_expression =
  ez_t_record ~loc ?core @@ List.mapi ~f:(fun i t -> Label.of_int i, t) xs


let t_sum ~loc ?core ~layout fields : type_expression =
  t_sum ~loc { fields; layout } ?type_meta:core ()


let t_sum_ez ~loc ?core ?(layout = default_layout) (lst : (string * type_expression) list)
    : type_expression
  =
  let fields = List.map ~f:(fun (x, y) -> Label.of_string x, y) lst in
  let layout = layout @@ fields_with_no_annot fields in
  let row = Row.of_alist_exn ~layout fields in
  make_t ~loc (T_sum row) core


let t_record_ez
    ~loc
    ?core
    ?(layout = default_layout)
    (lst : (string * type_expression) list)
    : type_expression
  =
  let lst = List.map ~f:(fun (x, y) -> Label.of_string x, y) lst in
  ez_t_record ~loc ?core ~layout lst


let t_bool ~loc ?core () : type_expression =
  t_sum_ez ~loc ?core [ "True", t_unit ~loc (); "False", t_unit ~loc () ]


let t_option ~loc ?core typ : type_expression =
  t_sum_ez ~loc ?core [ "Some", typ; "None", t_unit ~loc () ]


let t_option_abst ~loc ?core () : type_expression =
  let ty_binder = Type_var.of_input_var ~loc "'a" in
  t_abstraction
    ~loc
    { ty_binder
    ; kind = Type
    ; type_ =
        t_sum_ez
          ~loc
          ?core
          [ "Some", t_variable ~loc ty_binder (); "None", t_unit ~loc () ]
    }
    ()


(* types specific to LIGO test framework*)
let t_michelson_code ~loc ?core () : type_expression =
  t_constant ~loc ?core Literal_types.Michelson_program []


let t_test_exec_error ~loc ?core () : type_expression =
  t_sum_ez
    ~loc
    ?core
    [ "Rejected", t_pair ~loc (t_michelson_code ~loc ()) (t_address ~loc ())
    ; ( "Balance_too_low"
      , t_record_ez
          ~loc
          [ "contract_too_low", t_address ~loc ()
          ; "contract_balance", t_mutez ~loc ()
          ; "spend_request", t_mutez ~loc ()
          ] )
    ; "Other", t_string ~loc ()
    ]


let t_test_exec_result ~loc ?core () : type_expression =
  t_sum_ez ~loc ?core [ "Success", t_nat ~loc (); "Fail", t_test_exec_error ~loc () ]


let t_test_baker_policy ~loc ?core () : type_expression =
  t_sum_ez
    ~loc
    ?core
    [ "By_round", t_int ~loc ()
    ; "By_account", t_address ~loc ()
    ; "Excluding", t_list ~loc (t_address ~loc ())
    ]


let t_arrow param result ~loc ?s () : type_expression =
  t_arrow ~loc ?type_meta:s { type1 = param; type2 = result } ()


let t_shallow_closure param result ~loc ?s () : type_expression =
  make_t ~loc (T_arrow { type1 = param; type2 = result }) s


let t_chest_opening_result ~loc ?core () : type_expression =
  t_sum_ez
    ~loc
    ?core
    [ "Ok_opening", t_bytes ~loc ()
    ; "Fail_decrypt", t_unit ~loc ()
    ; "Fail_timelock", t_unit ~loc ()
    ]


let get_lambda_with_type e =
  match e.expression_content, e.type_expression.type_content with
  | E_lambda l, T_arrow { type1; type2 } -> Some (l, (type1, type2))
  | _ -> None


let get_t_bool (t : type_expression) : unit option =
  (* location of [t_bool] doesn't matter here since equality on type content 
       is modulo loc *)
  let t_bool = t_bool ~loc:t.location () in
  Option.some_if (Types.equal_type_content t.type_content t_bool.type_content) ()


let get_t_option (t : type_expression) : type_expression option =
  match t.type_content with
  | T_sum { fields; _ } ->
    let keys = Map.key_set fields in
    if Set.length keys = 2 && Set.mem keys (Label "Some") && Set.mem keys (Label "None")
    then Map.find fields (Label "Some")
    else None
  | _ -> None


let get_param_inj (t : type_expression)
    : (string * Literal_types.t * type_expression list) option
  =
  match t.type_content with
  | T_constant { language; injection; parameters } ->
    Some (language, injection, parameters)
  | _ -> None


let get_t_inj (t : type_expression) (v : Literal_types.t) : type_expression list option =
  match t.type_content with
  | T_constant { language = _; injection; parameters }
    when Literal_types.equal injection v -> Some parameters
  | _ -> None


let get_t_base_inj (t : type_expression) (v : Literal_types.t) : unit option =
  match get_t_inj t v with
  | Some [] -> Some ()
  | _ -> None


let get_t_unary_inj (t : type_expression) (v : Literal_types.t) : type_expression option =
  match get_t_inj t v with
  | Some [ a ] -> Some a
  | _ -> None


let get_t_binary_inj (t : type_expression) (v : Literal_types.t)
    : (type_expression * type_expression) option
  =
  match get_t_inj t v with
  | Some [ a; b ] -> Some (a, b)
  | _ -> None


let get_t__type_ (t : type_expression) : unit option = get_t_base_inj t _type_
  [@@map
    _type_
    , ( "int"
      , "nat"
      , "unit"
      , "tez"
      , "timestamp"
      , "address"
      , "bytes"
      , "string"
      , "key"
      , "signature"
      , "key_hash"
      , "chest"
      , "chest_key"
      , "michelson_program"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr" )]


let get_t__type_ (t : type_expression) : type_expression option = get_t_unary_inj t _type_
  [@@map
    _type_, ("contract", "list", "set", "ticket", "sapling_state", "sapling_transaction")]


let get_t_mutez (t : type_expression) : unit option = get_t_tez t
let get_t_michelson_code (t : type_expression) : unit option = get_t_michelson_program t
let tuple_of_record row = Row.to_tuple row

let get_t_tuple (t : type_expression) : type_expression list option =
  match t.type_content with
  | T_record row -> Some (tuple_of_record row)
  | _ -> None


let get_t_pair (t : type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_record m ->
    (match tuple_of_record m with
    | [ t1; t2 ] -> Some (t1, t2)
    | _ -> None)
  | _ -> None


let get_t_or (t : type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_sum row ->
    (match Map.to_alist row.fields with
    | [ (Label "left", l); (Label "right", r) ]
    | [ (Label "right", r); (Label "left", l) ] -> Some (l, r)
    | _ -> None)
  | _ -> None


let get_t_map (t : type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant { language = _; injection; parameters = [ k; v ] }
    when Literal_types.equal injection Literal_types.Map -> Some (k, v)
  | _ -> None


let get_t_typed_address (t : type_expression) : (type_expression * type_expression) option
  =
  match t.type_content with
  | T_constant { language = _; injection; parameters = [ k; v ] }
    when Literal_types.equal injection Literal_types.Typed_address -> Some (k, v)
  | _ -> None


let get_t_big_map (t : type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant { language = _; injection; parameters = [ k; v ] }
    when Literal_types.equal injection Literal_types.Big_map -> Some (k, v)
  | _ -> None


let get_t__type__exn t =
  match get_t__type_ t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))
  [@@map _type_, ("list", "set", "map", "typed_address", "big_map")]


let assert_t_contract (t : type_expression) : unit option =
  match get_t_unary_inj t Literal_types.Contract with
  | Some _ -> Some ()
  | _ -> None


let is_t__type_ t = Option.is_some (get_t__type_ t)
  [@@map
    _type_
    , ( "list"
      , "set"
      , "nat"
      , "string"
      , "bytes"
      , "int"
      , "unit"
      , "address"
      , "tez"
      , "contract"
      , "map"
      , "big_map" )]


let is_t_mutez t = is_t_tez t

let assert_t_list_operation (t : type_expression) : unit option =
  match get_t_list t with
  | Some t' -> get_t_base_inj t' Literal_types.Operation
  | None -> None


let assert_t__type_ : type_expression -> unit option = fun t -> get_t__type_ t
 [@@map
   _type_
   , ( "int"
     , "nat"
     , "unit"
     , "mutez"
     , "key"
     , "signature"
     , "key_hash"
     , "bytes"
     , "string"
     , "michelson_code" )]


let assert_t__type_ : type_expression -> unit option =
 fun v -> Option.map ~f:(fun _ -> ()) @@ get_t__type_ v
 [@@map _type_, ("set", "list")]


let ez_e_record (lst : (Label.t * expression) list) : expression_content =
  E_record (Record.of_list lst)


let e__ct_ () : expression_content = E_constant { cons_name = C__CT_; arguments = [] }
  [@@map _ct_, ("none", "nil", "set_empty", "map_empty", "big_map_empty")]


let e__ct_ p : expression_content = E_constant { cons_name = C__CT_; arguments = [ p ] }
  [@@map _ct_, ("some", "test_nil_views")]


let e__ct_ p p' : expression_content =
  E_constant { cons_name = C__CT_; arguments = [ p; p' ] }
  [@@map _ct_, ("cons", "set_add", "map_remove")]


let e__ct_ p p' p'' : expression_content =
  E_constant { cons_name = C__CT_; arguments = [ p; p'; p'' ] }
  [@@map _ct_, ("map_add", "test_cons_views")]


let e__type_ p : expression_content = E_literal (Literal__type_ p)
  [@@map
    _type_
    , ( "int"
      , "nat"
      , "mutez"
      , "string"
      , "bytes"
      , "timestamp"
      , "address"
      , "signature"
      , "key"
      , "key_hash"
      , "chain_id"
      , "operation"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr" )]


let e_unit () : expression_content = E_literal Literal_unit
let e_pair a b : expression_content = ez_e_record [ Label.of_int 0, a; Label.of_int 1, b ]

let e_bool ~loc b : expression_content =
  if b
  then
    E_constructor
      { constructor = Label "True"; element = make_e ~loc (e_unit ()) (t_unit ~loc ()) }
  else
    E_constructor
      { constructor = Label "False"; element = make_e ~loc (e_unit ()) (t_unit ~loc ()) }


let e_a_literal ~loc l t = make_e ~loc (E_literal l) t

let e_a__type_ ~loc p = make_e ~loc (e__type_ p) (t__type_ ~loc ())
  [@@map
    _type_
    , ( "unit"
      , "int"
      , "nat"
      , "mutez"
      , "timestamp"
      , "key_hash"
      , "string"
      , "bytes"
      , "address"
      , "key"
      , "signature"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr" )]


let e_a_pair ~loc a b =
  make_e ~loc (e_pair a b) (t_pair ~loc a.type_expression b.type_expression)


let e_a_constructor ~loc constructor element t =
  e_constructor ~loc { constructor = Label constructor; element } t


let e_a_record ~loc ?(layout = default_layout) r =
  e_record
    ~loc
    r
    (t_record
       ~loc
       ~layout:(layout @@ fields_with_no_annot (Map.to_alist r))
       (Map.map r ~f:(fun expr -> expr.type_expression)))


let ez_e_a_record ~loc ?layout r =
  make_e
    ~loc
    (ez_e_record r)
    (ez_t_record
       ~loc
       ?layout
       (List.map r ~f:(fun (name, expr) -> name, expr.type_expression)))


let e_a_variable ~loc v ty = e_variable ~loc v ty
let e_a_application ~loc lamb args t = e_application ~loc { lamb; args } t
let e_a_lambda ~loc l in_ty out_ty = e_lambda ~loc l (t_arrow ~loc in_ty out_ty ())
let e_a_recursive ~loc l = e_recursive ~loc l l.fun_type

let e_a_let_in ~loc let_binder rhs let_result attributes =
  e_let_in ~loc { let_binder; rhs; let_result; attributes } (get_type let_result)


let e_a_matching ~loc matchee cases t = e_matching ~loc { matchee; cases } t
let e_a_raw_code ~loc language code t = e_raw_code ~loc { language; code } t
let e_a_type_inst ~loc forall type_ u = e_type_inst ~loc { forall; type_ } u

let e_a_mod_in ~loc module_binder rhs let_result =
  e_mod_in ~loc { module_binder; rhs; let_result } (get_type let_result)


(* Constants *)
let e_a_nil ~loc t = make_e ~loc (e_nil ()) (t_list ~loc t)
let e_a_cons ~loc hd tl = make_e ~loc (e_cons hd tl) (t_list ~loc hd.type_expression)

let e_a_test_nil_views ~loc s =
  make_e ~loc (e_test_nil_views (e_a_unit ~loc ())) (t_views ~loc s)


let e_a_test_cons_views ~loc s name hd tl =
  make_e ~loc (e_test_cons_views name hd tl) (t_views ~loc s)


let e_a_set_empty ~loc t = make_e ~loc (e_set_empty ()) (t_set ~loc t)
let e_a_set_add ~loc hd tl = make_e ~loc (e_set_add hd tl) (t_set ~loc hd.type_expression)
let e_a_map_empty ~loc kt vt = make_e ~loc (e_map_empty ()) (t_map ~loc kt vt)

let e_a_map_add ~loc k v tl =
  make_e ~loc (e_map_add k v tl) (t_map ~loc k.type_expression v.type_expression)


let e_a_big_map_empty ~loc kt vt = make_e (e_big_map_empty ()) (t_big_map ~loc kt vt)

let e_a_big_map_add ~loc k v tl =
  make_e ~loc (e_map_add k v tl) (t_big_map ~loc k.type_expression v.type_expression)


let e_a_big_map_remove ~loc k tl = make_e ~loc (e_map_remove k tl) tl.type_expression

let get_a_int (t : expression) =
  match t.expression_content with
  | E_literal (Literal_int n) -> Some n
  | _ -> None


let get_a_string (t : expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None


let get_a_verbatim (t : expression) =
  match t.expression_content with
  | E_literal (Literal_string (Verbatim v)) -> Some v
  | _ -> None


let get_a_unit (t : expression) =
  match t.expression_content with
  | E_literal Literal_unit -> Some ()
  | _ -> None


let get_record_field_type (t : type_expression) (label : Label.t) : type_expression option
  =
  match get_t_record t with
  | None -> None
  | Some record -> Label.Map.find record.fields label


let get_record_fields (t : type_expression) : (Label.t * type_expression) list option =
  match get_t_record t with
  | None -> None
  | Some record ->
    let lst = Record.to_list record.fields in
    Some (List.map ~f:(fun (k, x) -> k, x) lst)


let get_e_tuple t =
  match t with
  | E_record r -> Some (List.map ~f:snd @@ Record.tuple_of_record r)
  | _ -> None


let get_sum_label_type (t : type_expression) (label : Label.t) : type_expression option =
  match get_t_sum t with
  | None -> None
  | Some s -> Label.Map.find s.fields label


(* getter for a function of the form p * s -> ret *)
let get_view_form ty =
  match get_t_arrow ty with
  | Some { type1 = tin; type2 = return } ->
    (match get_t_tuple tin with
    | Some [ arg; storage ] -> Some (arg, storage, return)
    | _ -> None)
  | None -> None

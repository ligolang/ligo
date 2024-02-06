module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim
open Literal_types
open Types

(* Helpers for accessing and constructing elements are derived using
   `ppx_woo` (`@@deriving ez`) *)
type signature_sort = [%import: Types.signature_sort] [@@deriving ez]
type signature = [%import: Types.signature] [@@deriving ez]

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
          , fun ~loc type_content : type_expression ->
              { type_content; location = loc; orig_var = None } )
        ; ("get", fun x -> x.type_content)
        ]
    ; wrap_constructor =
        ("type_content", fun type_content ~loc () -> make_t ~loc type_content)
    ; wrap_get = "type_content", get
    ; default_get = `Option
    }]

let t_constant ~loc injection parameters : type_expression =
  make_t ~loc (T_constant { language = Backend.Michelson.name; injection; parameters })


(* TODO?: X_name here should be replaced by X_injection *)
let t__type_ ~loc () : type_expression = t_constant ~loc _type_ []
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
      , "tx_rollup_l2_address"
      , "int64" )]


let t__type_ ~loc t : type_expression = t_constant ~loc _type_ [ t ]
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


let t__type_ ~loc t1 t2 : type_expression = t_constant ~loc _type_ [ t1; t2 ]
  [@@map _type_, ("big_map", "michelson_contract")]


let t_mutez = t_tez

let t_abstraction1 ~loc name kind : type_expression =
  let ty_binder = Type_var.fresh ~loc ~name:"_a" () in
  let type_ = t_constant ~loc name [ t_variable ~loc ty_binder () ] in
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


let t_abstraction3 ~loc name kind_l kind_m kind_r : type_expression =
  let ty_binder_l = Type_var.fresh ~loc ~name:"_l" () in
  let ty_binder_m = Type_var.fresh ~loc ~name:"_m" () in
  let ty_binder_r = Type_var.fresh ~loc ~name:"_r" () in
  let type_ =
    t_constant
      ~loc
      name
      [ t_variable ~loc ty_binder_l ()
      ; t_variable ~loc ty_binder_m ()
      ; t_variable ~loc ty_binder_r ()
      ]
  in
  t_abstraction
    ~loc
    { ty_binder = ty_binder_l
    ; kind = kind_l
    ; type_ =
        t_abstraction
          ~loc
          { ty_binder = ty_binder_m
          ; kind = kind_m
          ; type_ =
              t_abstraction ~loc { ty_binder = ty_binder_r; kind = kind_r; type_ } ()
          }
          ()
    }
    ()


let t_for_all ty_binder kind type_ = t_for_all { ty_binder; kind; type_ } ()

let t_record ~loc ~layout fields : type_expression =
  t_record ~loc (Row.create ~layout fields) ()


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

let ez_t_record ~loc ?(layout = default_layout) (lst : (Label.t * type_expression) list)
    : type_expression
  =
  let layout = layout @@ fields_with_no_annot lst in
  let row = Row.of_alist_exn ~layout lst in
  make_t ~loc (T_record row)


let t_pair ~loc a b : type_expression =
  ez_t_record ~loc [ Label.of_int 0, a; Label.of_int 1, b ]


let t_sum_ez ~loc ?(layout = default_layout) (lst : (string * type_expression) list)
    : type_expression
  =
  let fields = List.map ~f:(fun (x, y) -> Label.of_string x, y) lst in
  let layout = layout @@ fields_with_no_annot fields in
  let row = Row.of_alist_exn ~layout fields in
  make_t ~loc (T_sum row)


let t_bool ~loc () : type_expression =
  t_sum_ez ~loc [ "True", t_unit ~loc (); "False", t_unit ~loc () ]


let t_arrow param result ~loc ?(param_names = []) () : type_expression =
  t_arrow ~loc { type1 = param; type2 = result; param_names } ()


let get_lambda_with_type e =
  match e.expression_content, e.type_expression.type_content with
  | E_lambda l, T_arrow { type1; type2; param_names = _ } -> Some (l, (type1, type2))
  | _ -> None


let get_t_bool (t : type_expression) : unit option =
  match t.type_content with
  | T_sum { fields; _ } ->
    let keys = Map.key_set fields in
    if Set.length keys = 2
       && Set.mem keys (Label.of_string "True")
       && Set.mem keys (Label.of_string "False")
    then Some ()
    else None
  | _ -> None


let get_t_option (t : type_expression) : type_expression option =
  match t.type_content with
  | T_sum { fields; _ } ->
    let keys = Map.key_set fields in
    if Set.length keys = 2
       && Set.mem keys (Label.of_string "Some")
       && Set.mem keys (Label.of_string "None")
    then Map.find fields (Label.of_string "Some")
    else None
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
      , "michelson_program"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr" )]


let get_t__type_ (t : type_expression) : type_expression option = get_t_unary_inj t _type_
  [@@map
    _type_, ("contract", "list", "set", "ticket", "sapling_state", "sapling_transaction")]


let tuple_of_record row = Row.to_tuple row

let get_t_tuple (t : type_expression) : type_expression list option =
  match t.type_content with
  | T_record row when Row.is_tuple row -> Some (tuple_of_record row)
  | _ -> None


let get_t_pair (t : type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_record m when Row.is_tuple m ->
    (match tuple_of_record m with
    | [ t1; t2 ] -> Some (t1, t2)
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


let assert_t_list_operation (t : type_expression) : unit option =
  match get_t_list t with
  | Some t' -> get_t_base_inj t' Literal_types.Operation
  | None -> None


let ez_e_record (lst : (Label.t * expression) list) : expression_content =
  E_record (Record.of_list lst)


let e__ct_ arguments : expression_content = E_constant { cons_name = C__CT_; arguments }
  [@@map _ct_, "list_literal"]


let e__ct_ () : expression_content = E_constant { cons_name = C__CT_; arguments = [] }
  [@@map _ct_, "none"]


let e__ct_ p : expression_content = E_constant { cons_name = C__CT_; arguments = [ p ] }
  [@@map _ct_, ("some", "test_nil_views", "big_map_literal")]


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


let e_a_tuple ~loc lst =
  ez_e_a_record ~loc (List.mapi ~f:(fun i el -> Label.of_int i, el) lst)


let e_a_variable ~loc v ty = e_variable ~loc v ty
let e_a_application ~loc lamb args t = e_application ~loc { lamb; args } t

let e_a_application_exn ~loc lamb args =
  let Arrow.{ type1 = _; type2; param_names = _ } =
    get_t_arrow_exn lamb.type_expression
  in
  e_application ~loc { lamb; args } type2


let e_a_lambda ~loc l in_ty out_ty = e_lambda ~loc l (t_arrow ~loc in_ty out_ty ())

let rec e_a_applications ~loc lamb args : expression =
  match args with
  | [] -> lamb
  | args :: argss -> e_a_application_exn ~loc (e_a_applications ~loc lamb argss) args


let e_a_applications ~loc lamb args : expression =
  e_a_applications ~loc lamb (List.rev args)


let e_a_matching ~loc matchee cases t = e_matching ~loc { matchee; cases } t

let e_a_test_nil_views ~loc s =
  make_e ~loc (e_test_nil_views (e_a_unit ~loc ())) (t_views ~loc s)


let e_a_test_cons_views ~loc s name hd tl =
  make_e ~loc (e_test_cons_views name hd tl) (t_views ~loc s)


let get_record_fields (t : type_expression) : (Label.t * type_expression) list option =
  match get_t_record t with
  | None -> None
  | Some record ->
    let lst = Record.to_list record.fields in
    Some (List.map ~f:(fun (k, x) -> k, x) lst)


(* getter for a function of the form p * s -> ret *)
let get_view_form ty =
  match get_t_arrow ty with
  | Some { type1 = tin; type2 = return; param_names = _ } ->
    (match get_t_tuple tin with
    | Some [ arg; storage ] -> Some (arg, storage, return)
    | _ -> None)
  | None -> None

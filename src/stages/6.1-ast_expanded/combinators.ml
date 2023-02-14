module Ligo_string = Simple_utils.Ligo_string
open Ligo_prim
open Literal_types
open Types

[@@@warning "-32"]

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
          , fun ~loc ?source_type type_content : type_expression ->
              { type_content; location = loc; orig_var = None; source_type } )
        ; ("get", fun x -> x.type_content)
        ]
    ; wrap_constructor =
        ( "type_content"
        , fun type_content ~loc ?source_type () -> make_t ~loc ?source_type type_content
        )
    ; wrap_get = "type_content", get
    ; default_get = `Option
    }]

let t_constant ~loc injection parameters : type_expression =
  t_constant ~loc { language = Backend.Michelson.name; injection; parameters } ()


let get_t_inj (t : type_expression) (v : Ligo_prim.Literal_types.t)
    : type_expression list option
  =
  match t.type_content with
  | T_constant { language = _; injection; parameters }
    when Ligo_prim.Literal_types.equal injection v -> Some parameters
  | _ -> None


let get_t_unary_inj (t : type_expression) (v : Ligo_prim.Literal_types.t)
    : type_expression option
  =
  match get_t_inj t v with
  | Some [ a ] -> Some a
  | _ -> None


let get_t_map (t : type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant { language = _; injection; parameters = [ k; v ] }
    when Ligo_prim.Literal_types.equal injection Ligo_prim.Literal_types.Map -> Some (k, v)
  | _ -> None


let t__type_ ~loc () : type_expression = t_constant ~loc _type_ []
  [@@map
    _type_
    , ( "list"
      , "set"
      , "signature"
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
      , "chest" )]


let t__type_ ~loc t t' : type_expression = t_constant ~loc _type_ [ t; t' ]
  [@@map _type_, "map"]


let get_t__type_ (t : type_expression) : type_expression option = get_t_unary_inj t _type_
  [@@map
    _type_
    , ( "list"
      , "set"
      , "signature"
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
      , "chest" )]


let is_t__type_ t = Option.is_some (get_t__type_ t)
  [@@map
    _type_
    , ( "map"
      , "list"
      , "set"
      , "signature"
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
      , "chest" )]


let get_t__type__exn t =
  match get_t__type_ t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))
  [@@map
    _type_
    , ( "map"
      , "list"
      , "set"
      , "signature"
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
      , "chest" )]


let t_arrow param result ~loc ?source_type () : type_expression =
  t_arrow ~loc ?source_type { type1 = param; type2 = result } ()


let default_layout = Layout.default

let fields_with_no_annot fields =
  List.map ~f:(fun (name, _) -> Layout.{ name; annot = None }) fields


let t_record ~loc ~layout fields : type_expression =
  make_t ~loc (T_record { fields; layout })


let ez_t_record ~loc ?(layout = default_layout) lst : type_expression =
  let layout = layout @@ fields_with_no_annot lst in
  let row = Row.of_alist_exn ~layout lst in
  make_t ~loc (T_record row)


let make_t_ez_record
    ~loc
    ?(layout = default_layout)
    (lst : (string * type_expression) list)
    : type_expression
  =
  let lst = List.map ~f:(fun (name, t) -> Label.of_string name, t) lst in
  ez_t_record ~loc ~layout lst


let get_a_string (t : expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None


let e_a_variable v ty = e_variable v ty

let get_t_option (t : type_expression) : type_expression option =
  let l_none = Label.of_string "None" in
  let l_some = Label.of_string "Some" in
  match t.type_content with
  | T_sum { fields; _ } ->
    let keys = Record.labels fields in
    (match keys with
    | [ a; b ]
      when (Label.equal a l_none && Label.equal b l_some)
           || (Label.equal a l_some && Label.equal b l_none) ->
      Record.find_opt fields l_some
    | _ -> None)
  | _ -> None


let e_a_let_mut_in ~loc x = e_let_mut_in ~loc x (get_type x.let_result)
let e_a_let_in ~loc x = e_let_in ~loc x (get_type x.let_result)

let get_sum_type (t : type_expression) (label : Label.t) : type_expression =
  match get_t_sum t with
  | None -> failwith "pattern expanded: could not get sum type"
  | Some row ->
    (match Map.find row.fields label with
    | None -> failwith "pattern expanded: could not get row from its label"
    | Some elem_type -> elem_type)


let t_sum ~loc ~layout fields : type_expression = make_t ~loc (T_sum { fields; layout })

let t_sum_ez ~loc ?(layout = default_layout) (lst : (string * type_expression) list)
    : type_expression
  =
  let lst = List.map ~f:(fun (name, t) -> Label.of_string name, t) lst in
  let layout = layout @@ fields_with_no_annot lst in
  let map = Row.of_alist_exn ~layout lst in
  make_t ~loc (T_sum map)


let t_bool ~loc () : type_expression =
  t_sum_ez ~loc [ "True", t_unit ~loc (); "False", t_unit ~loc () ]


let get_t_bool (t : type_expression) : unit option =
  let t_bool = t_bool ~loc:t.location () in
  Option.some_if (equal_type_content t.type_content t_bool.type_content) ()


let t_option ~loc typ : type_expression =
  t_sum_ez ~loc [ "Some", typ; "None", t_unit ~loc () ]


let t_record ~loc ~layout fields : type_expression =
  make_t ~loc (T_record { fields; layout })


let ez_t_record ~loc ?(layout = default_layout) lst : type_expression =
  let layout = layout @@ fields_with_no_annot lst in
  let row = Row.of_alist_exn ~layout lst in
  make_t ~loc (T_record row)


let t_pair ~loc a b : type_expression =
  ez_t_record ~loc [ Label.of_int 0, a; Label.of_int 1, b ]


let is_michelson_or (fields : _ Record.t) (layout : Ligo_prim.Layout.t) =
  match layout with
  | Inner [ Field { name = _; annot = ann_a }; Field { name = _; annot = ann_b } ] ->
    (match
       Record.find_opt fields (Label "M_left"), Record.find_opt fields (Label "M_right")
     with
    | Some l, Some r -> Some ((l, ann_a), (r, ann_b))
    | _ -> None)
  | _ -> None


let is_michelson_pair (fields : _ Record.t) (layout : Ligo_prim.Layout.t) =
  match layout with
  | Inner [ Field { name = _; annot = ann_a }; Field { name = _; annot = ann_b } ] ->
    (match Record.find_opt fields (Label "0"), Record.find_opt fields (Label "1") with
    | Some l, Some r -> Some ((l, ann_a), (r, ann_b))
    | _ -> None)
  | _ -> None


let e_unit () : expression_content = E_literal Literal_unit

let get_e_tuple t =
  match t with
  | E_record r -> Some (List.map ~f:snd @@ Record.tuple_of_record r)
  | _ -> None


let e_bool ~loc b : expression_content =
  if b
  then
    E_constructor
      { constructor = Label "True"; element = make_e ~loc (e_unit ()) (t_unit ~loc ()) }
  else
    E_constructor
      { constructor = Label "False"; element = make_e ~loc (e_unit ()) (t_unit ~loc ()) }

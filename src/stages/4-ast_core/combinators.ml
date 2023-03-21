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
          , fun ~loc ?sugar expression_content : expression ->
              { expression_content; location = loc; sugar } )
        ; ("get", fun x -> x.expression_content)
        ; ("get_sugar", fun x -> x.sugar)
        ]
    ; wrap_constructor =
        ( "expression_content"
        , fun expression_content ~loc ?sugar () -> make_e ~loc ?sugar expression_content
        )
    ; wrap_get = "expression_content", get
    }]

type type_content = [%import: Types.type_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ~loc ?sugar type_content : type_expression ->
              { type_content; location = loc; sugar } )
        ; ("get", fun x -> x.type_content)
        ]
    ; wrap_constructor =
        ( "type_content"
        , fun type_content ~loc ?sugar () -> make_t ~loc ?sugar type_content )
    ; wrap_get = "type_content", get
    ; default_get = `Option
    }]

let t_constant ~loc ?sugar type_operator arguments : type_expression =
  make_t
    ~loc
    ?sugar
    (T_app
       { type_operator =
           Module_access.make_el
           @@ Type_var.of_input_var ~loc (Literal_types.to_string type_operator)
       ; arguments
       })


let t_abstraction ~loc ?sugar ty_binder kind type_ =
  make_t ~loc ?sugar (T_abstraction { ty_binder; kind; type_ })


let t_for_all ~loc ?sugar ty_binder kind type_ =
  make_t ~loc ?sugar (T_for_all { ty_binder; kind; type_ })


(* TODO?: X_name here should be replaced by X_injection *)
let t__type_ ~loc ?sugar () : type_expression = t_constant ~loc ?sugar _type_ []
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
      , "chest" )]


let t__type_ ~loc ?sugar t : type_expression = t_constant ~loc ?sugar _type_ [ t ]
  [@@map _type_, ("list", "set", "contract", "ticket")]


let t__type_ ~loc ?sugar t t' : type_expression = t_constant ~loc ?sugar _type_ [ t; t' ]
  [@@map _type_, ("map", "big_map", "typed_address")]


let t_mutez = t_tez

let t_abstraction1 ~loc ?sugar name kind : type_expression =
  let ty_binder = Type_var.fresh ~loc ~name:"_a" () in
  let type_ = t_constant ~loc name [ t_variable ~loc ty_binder () ] in
  t_abstraction ~loc ?sugar ty_binder kind type_


let t_abstraction2 ~loc ?sugar name kind_l kind_r : type_expression =
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
    ?sugar
    ty_binder_l
    kind_l
    (t_abstraction ~loc ty_binder_r kind_r type_)


let t_record ~loc ?sugar ?layout fields : type_expression =
  make_t ~loc ?sugar @@ T_record (Row.create ~layout fields)


let default_layout fields : Layout.t = Layout.comb fields

let ez_t_record ~loc ?sugar ?layout lst : type_expression =
  let row = Row.of_alist_exn ~layout lst in
  make_t ~loc ?sugar @@ T_record row


let make_t_ez_record ~loc ?sugar ?layout (lst : (string * type_expression) list)
    : type_expression
  =
  let lst = List.map ~f:(fun (x, y) -> Label.of_string x, y) lst in
  ez_t_record ~loc ?sugar ?layout lst


let t_pair ~loc ?sugar a b : type_expression =
  ez_t_record ~loc ?sugar [ Label.of_int 0, a; Label.of_int 1, b ]


let t_sum ~loc ?sugar ?layout fields : type_expression =
  make_t ~loc ?sugar @@ T_sum (Row.create ~layout fields)


let ez_t_sum ~loc ?sugar ?layout lst =
  (* inconsistent naming conventions, but [t_sum_ez] is already taken *)
  let row = Row.of_alist_exn ~layout lst in
  make_t ~loc ?sugar @@ T_sum row


let t_sum_ez ~loc ?sugar ?layout (lst : (string * type_expression) list) : type_expression
  =
  (* this should be [make_t_ez_sum] if we want to be consistent *)
  let lst = List.map ~f:(fun (x, y) -> Label.of_string x, y) lst in
  ez_t_sum ~loc ?sugar ?layout lst


let t_bool ~loc ?sugar () : type_expression =
  t_sum_ez ~loc ?sugar [ "True", t_unit ~loc (); "False", t_unit ~loc () ]


let t_arrow ~loc ?sugar param result : type_expression =
  t_arrow ~loc ?sugar { type1 = param; type2 = result } ()


let t_shallow_closure ~loc ?sugar param result : type_expression =
  make_t ~loc ?sugar (T_arrow { type1 = param; type2 = result })


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


(* wtf why is this 'record'? should be 'row' *)
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


let ez_e_record ~loc (lst : (Label.t * expression) list) : expression =
  let map = Record.of_list lst in
  e_record ~loc map ()


let e_var ~loc ?sugar n : expression =
  e_variable (Value_var.of_input_var ~loc n) ~loc ?sugar ()


let e_unit ~loc ?sugar () : expression = e_literal Literal_unit ~loc ?sugar ()
let e_literal ~loc ?sugar l : expression = e_literal l ~loc ?sugar ()

let e__type_ ~loc ?sugar p : expression =
  make_e ~loc ?sugar @@ E_literal (Literal__type_ p)
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


let e'_bytes b : expression_content =
  let bytes = Hex.to_bytes (`Hex b) in
  E_literal (Literal_bytes bytes)


let e_bytes_hex ~loc ?sugar b : expression =
  let e' = e'_bytes b in
  make_e ~loc ?sugar e'


let e_bytes_raw ~loc ?sugar (b : bytes) : expression =
  make_e ~loc ?sugar @@ E_literal (Literal_bytes b)


let e_bytes_string ~loc ?sugar (s : string) : expression =
  make_e ~loc ?sugar @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))


let e_constant ~loc ?sugar cons_name arguments =
  e_constant ~loc ?sugar { cons_name; arguments } ()


let e_variable ~loc v : expression = e_variable ~loc v ()
let e_application ~loc lamb args : expression = e_application ~loc { lamb; args } ()

let e_lambda ~loc ?sugar binder output_type result =
  e_lambda ~loc ?sugar { binder; output_type; result } ()


let e_type_abs ~loc ?sugar type_binder result =
  e_type_abstraction ~loc ?sugar { type_binder; result } ()


let e_recursive ~loc ?sugar ?(force_lambdarec = false) fun_name fun_type lambda =
  e_recursive ~loc ?sugar { fun_name; fun_type; lambda; force_lambdarec } ()


let e_let_in ~loc ?sugar let_binder rhs let_result attributes =
  e_let_in ~loc ?sugar { let_binder; rhs; let_result; attributes } ()


let e_let_mut_in ~loc ?sugar let_binder rhs let_result attributes =
  e_let_mut_in ~loc ?sugar { let_binder; rhs; let_result; attributes } ()


let e_type_in type_binder rhs let_result = e_type_in { type_binder; rhs; let_result } ()

let e_mod_in ~loc ?sugar module_binder rhs let_result =
  e_mod_in ~loc ?sugar { module_binder; rhs; let_result } ()


let e_raw_code ~loc ?sugar language code = e_raw_code ~loc ?sugar { language; code } ()

let e_constructor ~loc constructor element : expression =
  e_constructor ~loc { constructor; element } ()


let e_matching ~loc ?sugar matchee cases : expression =
  e_matching ~loc ?sugar { matchee; cases } ()


let e_record_accessor ~loc ?sugar struct_ path =
  e_accessor ~loc ?sugar ({ struct_; path } : _ Types.Accessor.t) ()


let e_record_update ~loc ?sugar struct_ path update =
  e_update ~loc ?sugar ({ struct_; path; update } : _ Types.Update.t) ()


let e_module_accessor ~loc ?sugar module_path element =
  e_module_accessor ~loc ?sugar { module_path; element } ()


let e_ascription ~loc ?sugar anno_expr type_annotation : expression =
  e_ascription ~loc ?sugar { anno_expr; type_annotation } ()


let e_lambda_ez ~loc var ?ascr ?mut_flag output_type result : expression =
  e_lambda ~loc (Param.make ?mut_flag var ascr) output_type result


let e_let_in_ez ~loc var ?ascr ?(mut = false) attributes rhs let_result =
  let binder = Pattern.var ~loc (Binder.make var ascr) in
  if mut
  then e_let_mut_in ~loc binder attributes rhs let_result
  else e_let_in ~loc binder attributes rhs let_result


(* Constants *)
let e_some ~loc ?sugar s : expression = e_constant ~loc ?sugar C_SOME [ s ]
let e_none ~loc ?sugar () : expression = e_constant ~loc ?sugar C_NONE []

let e_string_cat ~loc ?sugar sl sr : expression =
  e_constant ~loc ?sugar C_CONCAT [ sl; sr ]


let e_map_add ~loc ?sugar k v old : expression =
  e_constant ~loc ?sugar C_MAP_ADD [ k; v; old ]


let e_bool ~loc b : expression =
  if b
  then
    e_constructor
      ~loc
      (Label "True")
      (e_ascription ~loc (e_unit ~loc ()) (t_unit ~loc ()))
  else
    e_constructor
      ~loc
      (Label "False")
      (e_ascription ~loc (e_unit ~loc ()) (t_unit ~loc ()))


let get_e_int (t : expression) =
  match t.expression_content with
  | E_literal (Literal_int n) -> Some n
  | _ -> None


let get_e_string (t : expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None


let get_e_verbatim (t : expression) =
  match t.expression_content with
  | E_literal (Literal_string (Verbatim v)) -> Some v
  | _ -> None


let get_e_unit (t : expression) =
  match t.expression_content with
  | E_literal Literal_unit -> Some ()
  | _ -> None


let get_e_pair t =
  match t with
  | E_record r ->
    let lst = Record.to_list r in
    (match lst with
    | [ (Label "0", a); (Label "1", b) ] | [ (Label "1", b); (Label "0", a) ] ->
      Some (a, b)
    | _ -> None)
  | _ -> None


let get_e_list t =
  let rec aux t =
    match t with
    | E_constant { cons_name = C_CONS; arguments = [ key; lst ] } ->
      let lst = aux lst.expression_content in
      Some key :: lst
    | E_constant { cons_name = C_LIST_EMPTY; arguments = [] } -> []
    | _ -> [ None ]
  in
  Option.all @@ aux t


let get_e_tuple t =
  match t with
  | E_record r -> Some (List.map ~f:snd @@ Record.tuple_of_record r)
  | _ -> None


let get_record_field_type (t : type_expression) (label : Label.t) : type_expression option
  =
  match get_t_record t with
  | None -> None
  | Some row ->
    (match Map.find row.fields label with
    | None -> None
    | Some elem_type -> Some elem_type)


let get_e_ascription a =
  match a with
  | E_ascription { anno_expr; type_annotation } -> Some (anno_expr, type_annotation)
  | _ -> None


let get_type_abstractions (e : expression) =
  let rec aux tv e =
    match get_e_type_abstraction e with
    | None -> tv, e
    | Some { type_binder; result } -> aux (type_binder :: tv) result
  in
  aux [] e


(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option =
 fun e ->
  match e.expression_content with
  | E_record record ->
    (match Record.to_list record with
    | [ (Label "0", a); (Label "1", b) ] | [ (Label "1", b); (Label "0", a) ] ->
      Some (a, b)
    | _ -> None)
  | _ -> None


let extract_record : expression -> (Label.t * expression) list option =
 fun e ->
  match e.expression_content with
  | E_record lst -> Some (Record.to_list lst)
  | _ -> None


let extract_map : expression -> (expression * expression) list option =
 fun e ->
  let rec aux e =
    match e.expression_content with
    | E_constant { cons_name = C_UPDATE | C_MAP_ADD; arguments = [ k; v; map ] } ->
      let map = aux map in
      Some (k, v) :: map
    | E_constant { cons_name = C_MAP_EMPTY | C_BIG_MAP_EMPTY; arguments = [] } -> []
    | _ -> [ None ]
  in
  Option.all @@ aux e


let t_michelson_sum ~loc l l_ann r r_ann =
  let l_label = Label.Label "M_left" in
  let r_label = Label.Label "M_right" in
  let layout =
    Layout.Inner
      [ Field Layout.{ name = l_label; annot = Some l_ann }
      ; Field { name = r_label; annot = Some r_ann }
      ]
  in
  ez_t_sum ~loc ~layout [ l_label, l; r_label, r ]


let t_michelson_pair ~loc l l_ann r r_ann =
  let l_label = Label.Label "0" in
  let r_label = Label.Label "1" in
  let layout =
    Layout.Inner
      [ Field Layout.{ name = l_label; annot = Some l_ann }
      ; Field { name = r_label; annot = Some r_ann }
      ]
  in
  ez_t_record ~loc ~layout [ l_label, l; r_label, r ]

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
          , fun ~loc expression_content : expression ->
              { expression_content; location = loc } )
        ; ("get", fun x -> x.expression_content)
        ]
    ; wrap_constructor =
        ( "expression_content"
        , fun expression_content ~loc () -> make_e ~loc expression_content )
    ; wrap_get = "expression_content", get
    }]

type type_content = [%import: Types.type_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ~loc type_content : type_expression -> { type_content; location = loc } )
        ; ("get", fun x -> x.type_content)
        ]
    ; wrap_constructor =
        ("type_content", fun type_content ~loc () -> make_t ~loc type_content)
    ; wrap_get = "type_content", get
    ; default_get = `Option
    }]

let t_constant ~loc type_operator arguments : type_expression =
  make_t
    ~loc
    (T_app
       { type_operator =
           Module_access.make_el
           @@ Type_var.of_input_var ~loc (Literal_types.to_string type_operator)
       ; arguments
       })


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
      , "baker_hash" )]


let ez_t_sum ~loc ?layout ?orig_name lst =
  (* inconsistent naming conventions, but [t_sum_ez] is already taken *)
  let layout =
    match layout with
    | Some x -> x
    | None -> Layout.default_of_labels (List.map ~f:fst lst)
  in
  let layout = Some layout in
  let row = Row.of_alist_exn ~layout lst in
  make_t ~loc @@ T_sum (row, orig_name)


let t_sum_ez ~loc ?layout (lst : (string * type_expression) list) : type_expression =
  (* this should be [make_t_ez_sum] if we want to be consistent *)
  let lst = List.map ~f:(fun (x, y) -> Label.of_string x, y) lst in
  ez_t_sum ~loc ?layout lst


let t_bool ~loc () : type_expression =
  t_sum_ez ~loc [ "False", t_unit ~loc (); "True", t_unit ~loc () ]


let get_t_bool (t : type_expression) : unit option =
  match t.type_content with
  | T_sum ({ fields; _ }, _) ->
    let keys = Map.key_set fields in
    if Set.length keys = 2
       && Set.mem keys (Label.of_string "True")
       && Set.mem keys (Label.of_string "False")
    then Some ()
    else None
  | _ -> None


let get_t_option (t : type_expression) : type_expression option =
  match t.type_content with
  | T_sum ({ fields; _ }, _) ->
    let keys = Map.key_set fields in
    if Set.length keys = 2
       && Set.mem keys (Label.of_string "Some")
       && Set.mem keys (Label.of_string "None")
    then Map.find fields (Label.of_string "Some")
    else None
  | _ -> None


let e_unit ~loc () : expression = e_literal Literal_unit ~loc ()
let e_literal ~loc l : expression = e_literal l ~loc ()

let e__type_ ~loc p : expression = make_e ~loc @@ E_literal (Literal__type_ p)
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


let e_constant ~loc cons_name arguments = e_constant ~loc { cons_name; arguments } ()
let e_variable ~loc v : expression = e_variable ~loc v ()
let e_application ~loc lamb args : expression = e_application ~loc { lamb; args } ()

let e_lambda ~loc binder output_type result =
  e_lambda ~loc { binder; output_type; result } ()


let e_type_abs ~loc type_binder result =
  e_type_abstraction ~loc { type_binder; result } ()


let e_recursive ~loc ?(force_lambdarec = false) fun_name fun_type lambda =
  e_recursive ~loc { fun_name; fun_type; lambda; force_lambdarec } ()


let e_let_in ~loc let_binder rhs let_result attributes =
  e_let_in ~loc { let_binder; rhs; let_result; attributes } ()


let e_let_mut_in ~loc let_binder rhs let_result attributes =
  e_let_mut_in ~loc { let_binder; rhs; let_result; attributes } ()


let e_mod_in ~loc module_binder rhs let_result =
  e_mod_in ~loc { module_binder; rhs; let_result } ()


let e_raw_code ~loc language code = e_raw_code ~loc { language; code } ()

let e_constructor ~loc constructor element : expression =
  e_constructor ~loc { constructor; element } ()


let e_matching ~loc ?disc_label matchee cases : expression =
  e_matching ~loc { matchee; disc_label; cases } ()


let e_record_accessor ~loc struct_ path =
  e_accessor ~loc ({ struct_; path } : _ Types.Accessor.t) ()


let e_record_update ~loc struct_ path update =
  e_update ~loc ({ struct_; path; update } : _ Types.Update.t) ()


let e_ascription ~loc anno_expr type_annotation : expression =
  e_ascription ~loc { anno_expr; type_annotation } ()


let get_e_tuple t =
  match t with
  | E_record r -> Some (List.map ~f:snd @@ Record.tuple_of_record r)
  | _ -> None


let get_e_application t =
  match t with
  | E_application { lamb; args } -> Some (lamb, args)
  | _ -> None


let rec get_e_applications t =
  match get_e_application t with
  | Some (lamb, args) ->
    (match get_e_applications lamb.expression_content with
    | [] -> [ lamb; args ]
    | apps -> apps @ [ args ])
  | None -> []


let get_e_ascription a =
  match a with
  | E_ascription { anno_expr; type_annotation } -> Some (anno_expr, type_annotation)
  | _ -> None


let get_e_lambda t =
  match t with
  | E_lambda lam -> Some lam
  | _ -> None


(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) option =
 fun e ->
  match e.expression_content with
  | E_record record ->
    (match Record.to_list record with
    | [ (Label ("0", _), a); (Label ("1", _), b) ]
    | [ (Label ("1", _), b); (Label ("0", _), a) ] -> Some (a, b)
    | _ -> None)
  | _ -> None


let extract_record : expression -> (Label.t * expression) list option =
 fun e ->
  match e.expression_content with
  | E_record lst -> Some (Record.to_list lst)
  | _ -> None


(** Removes [T_abstraction] and [T_for_all] from the top-level. *)
let rec strip_abstraction : ty_expr -> ty_expr =
 fun ty_expr ->
  match ty_expr.type_content with
  | T_abstraction { type_; ty_binder = _; kind = _ }
  | T_for_all { type_; ty_binder = _; kind = _ } ->
    let stripped = strip_abstraction type_ in
    { ty_expr with type_content = stripped.type_content }
  | T_variable _
  | T_constant _
  | T_sum _
  | T_record _
  | T_arrow _
  | T_singleton _
  | T_contract_parameter _
  | T_app _
  | T_module_accessor _ -> ty_expr

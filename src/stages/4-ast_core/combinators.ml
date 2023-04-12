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


let e_ascription ~loc ?sugar anno_expr type_annotation : expression =
  e_ascription ~loc ?sugar { anno_expr; type_annotation } ()


let get_e_tuple t =
  match t with
  | E_record r -> Some (List.map ~f:snd @@ Record.tuple_of_record r)
  | _ -> None


let get_e_ascription a =
  match a with
  | E_ascription { anno_expr; type_annotation } -> Some (anno_expr, type_annotation)
  | _ -> None


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

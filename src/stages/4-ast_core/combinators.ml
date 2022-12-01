open Types

(* Helpers for accessing and constructing elements are derived using
   `ppx_woo` (`@@deriving ez`) *)

type expression_content = [%import: Types.expression_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_e"
          , fun ?(loc = Location.generated) ?sugar expression_content : expression ->
              { expression_content; location = loc; sugar } )
        ; ("get", fun x -> x.expression_content)
        ; ("get_sugar", fun x -> x.sugar)
        ]
    ; wrap_constructor =
        ( "expression_content"
        , fun expression_content ?loc ?sugar () -> make_e ?loc ?sugar expression_content
        )
    ; wrap_get = "expression_content", get
    }]

type type_content = [%import: Types.type_content]
[@@deriving
  ez
    { prefixes =
        [ ( "make_t"
          , fun ?(loc = Location.generated) ?sugar type_content : type_expression ->
              { type_content; location = loc; sugar } )
        ; ("get", fun x -> x.type_content)
        ]
    ; wrap_constructor =
        ( "type_content"
        , fun type_content ?loc ?sugar () -> make_t ?loc ?sugar type_content )
    ; wrap_get = "type_content", get
    ; default_get = `Option
    }]

open Ligo_prim
open Ligo_prim.Literal_types

let t_constant ?loc ?sugar type_operator arguments : type_expression =
  make_t
    ?loc
    ?sugar
    (T_app
       { type_operator = Type_var.of_input_var (Literal_types.to_string type_operator)
       ; arguments
       })


let t_abstraction ?loc ?sugar ty_binder kind type_ =
  make_t ?loc ?sugar (T_abstraction { ty_binder; kind; type_ })


let t_for_all ?loc ?sugar ty_binder kind type_ =
  make_t ?loc ?sugar (T_for_all { ty_binder; kind; type_ })


(* TODO?: X_name here should be replaced by X_injection *)
let t__type_ ?loc ?sugar () : type_expression = t_constant ?loc ?sugar _type_ []
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


let t__type_ ?loc ?sugar t : type_expression = t_constant ?loc ?sugar _type_ [ t ]
  [@@map _type_, ("list", "set", "contract", "ticket")]


let t__type_ ?loc ?sugar t t' : type_expression = t_constant ?loc ?sugar _type_ [ t; t' ]
  [@@map _type_, ("map", "big_map", "typed_address")]


let t_mutez = t_tez

let t_abstraction1 ?loc ?sugar name kind : type_expression =
  let ty_binder = Type_var.fresh ~name:"_a" () in
  let type_ = t_constant name [ t_variable ty_binder () ] in
  t_abstraction ?loc ?sugar ty_binder kind type_


let t_abstraction2 ?loc ?sugar name kind_l kind_r : type_expression =
  let ty_binder_l = Type_var.fresh ~name:"_l" () in
  let ty_binder_r = Type_var.fresh ~name:"_r" () in
  let type_ = t_constant name [ t_variable ty_binder_l (); t_variable ty_binder_r () ] in
  t_abstraction
    ?loc
    ?sugar
    ty_binder_l
    kind_l
    (t_abstraction ?loc ty_binder_r kind_r type_)


let t_record ?loc ?sugar ?layout fields : type_expression =
  make_t ?loc ?sugar @@ T_record { fields; layout }


let default_layout : Layout.t = Layout.L_tree

let make_t_ez_record ?loc ?sugar ?layout (lst : (string * type_expression) list)
    : type_expression
  =
  let lst =
    List.mapi
      ~f:(fun i (x, y) ->
        ( Label.of_string x
        , ({ associated_type = y; michelson_annotation = None; decl_pos = i }
            : row_element) ))
      lst
  in
  let map = Record.of_list lst in
  t_record ?loc ?sugar ?layout map


let ez_t_record ?loc ?sugar ?(layout = default_layout) lst : type_expression =
  let m = Record.of_list lst in
  t_record ?loc ?sugar ~layout m


let t_pair ?loc ?sugar a b : type_expression =
  ez_t_record
    ?loc
    ?sugar
    [ Label.of_int 0, { associated_type = a; michelson_annotation = None; decl_pos = 0 }
    ; Label.of_int 1, { associated_type = b; michelson_annotation = None; decl_pos = 1 }
    ]


let t_sum ?loc ?sugar ?layout fields : type_expression =
  make_t ?loc ?sugar @@ T_sum { fields; layout }


let t_sum_ez ?loc ?sugar ?layout (lst : (string * type_expression) list) : type_expression
  =
  let lst =
    List.mapi
      ~f:(fun i (x, y) ->
        ( Label.of_string x
        , ({ associated_type = y; michelson_annotation = None; decl_pos = i }
            : row_element) ))
      lst
  in
  let map = Record.of_list lst in
  t_sum ?loc ?sugar ?layout map


let t_bool ?loc ?sugar () : type_expression =
  t_sum_ez ?loc ?sugar [ "True", t_unit (); "False", t_unit () ]


let t_arrow ?loc ?sugar param result : type_expression =
  t_arrow ?loc ?sugar { type1 = param; type2 = result } ()


let t_shallow_closure ?loc ?sugar param result : type_expression =
  make_t ?loc ?sugar (T_arrow { type1 = param; type2 = result })


let get_t_bool (t : type_expression) : unit option =
  match t.type_content with
  | t when Types.compare_type_content t (t_bool ()).type_content = 0 -> Some ()
  | _ -> None


let get_t_option (t : type_expression) : type_expression option =
  match t.type_content with
  | T_sum { fields; _ } ->
    let keys = Record.LMap.keys fields in
    (match keys with
    | [ Label "Some"; Label "None" ] | [ Label "None"; Label "Some" ] ->
      let some = Record.LMap.find (Label "Some") fields in
      Some some.associated_type
    | _ -> None)
  | _ -> None


let tuple_of_record (m : _ Rows.row_element_mini_c Record.t) =
  let aux i =
    let opt = Record.LMap.find_opt (Label.of_int i) m in
    Option.bind ~f:(fun opt -> Some (opt, i + 1)) opt
  in
  let l = Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux in
  List.map ~f:(fun Rows.{ associated_type; _ } -> associated_type) l


let get_t_tuple (t : type_expression) : type_expression list option =
  match t.type_content with
  | T_record struct_ -> Some (tuple_of_record struct_.fields)
  | _ -> None


let get_t_pair (t : type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_record m ->
    let lst = tuple_of_record m.fields in
    (match List.(length lst = 2) with
    | true -> Some List.(nth_exn lst 0, nth_exn lst 1)
    | false -> None)
  | _ -> None


let ez_e_record (lst : (Label.t * expression) list) : expression =
  let aux prev (k, v) = Record.LMap.add k v prev in
  let map = List.fold_left ~f:aux ~init:Record.LMap.empty lst in
  e_record map ()


let e_var ?loc ?sugar n : expression =
  e_variable (Value_var.of_input_var ?loc n) ?loc ?sugar ()


let e_unit ?loc ?sugar () : expression = e_literal Literal_unit ?loc ?sugar ()
let e_literal ?loc ?sugar l : expression = e_literal l ?loc ?sugar ()

let e__type_ ?loc ?sugar p : expression =
  make_e ?loc ?sugar @@ E_literal (Literal__type_ p)
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


let e_bytes_hex ?loc ?sugar b : expression =
  let e' = e'_bytes b in
  make_e ?loc ?sugar e'


let e_bytes_raw ?loc ?sugar (b : bytes) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes b)


let e_bytes_string ?loc ?sugar (s : string) : expression =
  make_e ?loc ?sugar @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))


let e_constant ?loc ?sugar cons_name arguments =
  e_constant ?loc ?sugar { cons_name; arguments } ()


let e_variable v : expression = e_variable v ()
let e_application lamb args : expression = e_application { lamb; args } ()

let e_lambda ?loc ?sugar binder output_type result =
  e_lambda ?loc ?sugar { binder; output_type; result } ()


let e_type_abs ?loc ?sugar type_binder result =
  e_type_abstraction ?loc ?sugar { type_binder; result } ()


let e_recursive ?loc ?sugar fun_name fun_type lambda =
  e_recursive ?loc ?sugar { fun_name; fun_type; lambda } ()


let e_let_in ?loc ?sugar let_binder rhs let_result attributes =
  e_let_in ?loc ?sugar { let_binder; rhs; let_result; attributes } ()


let e_let_mut_in ?loc ?sugar let_binder rhs let_result attributes =
  e_let_mut_in ?loc ?sugar { let_binder; rhs; let_result; attributes } ()


let e_type_in type_binder rhs let_result = e_type_in { type_binder; rhs; let_result } ()

let e_mod_in ?loc ?sugar module_binder rhs let_result =
  e_mod_in ?loc ?sugar { module_binder; rhs; let_result } ()


let e_raw_code ?loc ?sugar language code = e_raw_code ?loc ?sugar { language; code } ()

let e_constructor constructor element : expression =
  e_constructor { constructor; element } ()


let e_matching ?loc ?sugar matchee cases : expression =
  e_matching ?loc ?sugar { matchee; cases } ()


let e_record_accessor ?loc ?sugar struct_ path =
  e_accessor ?loc ?sugar ({ struct_; path } : _ Types.Accessor.t) ()


let e_record_update ?loc ?sugar struct_ path update =
  e_update ?loc ?sugar ({ struct_; path; update } : _ Types.Update.t) ()


let e_module_accessor ?loc ?sugar module_path element =
  e_module_accessor ?loc ?sugar { module_path; element } ()


let e_ascription ?loc ?sugar anno_expr type_annotation : expression =
  e_ascription ?loc ?sugar { anno_expr; type_annotation } ()


let e_lambda_ez ?loc var ?ascr ?mut_flag output_type result : expression =
  e_lambda ?loc (Ligo_prim.Param.make ?mut_flag var ascr) output_type result


let e_let_in_ez ?loc var ?ascr ?(mut = false) attributes rhs let_result =
  let binder = Types.Pattern.var (Binder.make var ascr) in
  if mut
  then e_let_mut_in ?loc binder attributes rhs let_result
  else e_let_in ?loc binder attributes rhs let_result


(* Constants *)
let e_some ?loc ?sugar s : expression = e_constant ?loc ?sugar C_SOME [ s ]
let e_none ?loc ?sugar () : expression = e_constant ?loc ?sugar C_NONE []

let e_string_cat ?loc ?sugar sl sr : expression =
  e_constant ?loc ?sugar C_CONCAT [ sl; sr ]


let e_map_add ?loc ?sugar k v old : expression =
  e_constant ?loc ?sugar C_MAP_ADD [ k; v; old ]


let e_bool b : expression =
  if b
  then e_constructor (Label "True") (e_ascription (e_unit ()) (t_unit ()))
  else e_constructor (Label "False") (e_ascription (e_unit ()) (t_unit ()))


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
    let lst = Record.LMap.to_kv_list_rev r in
    (match lst with
    | [ (Label "O", a); (Label "1", b) ] | [ (Label "1", b); (Label "0", a) ] ->
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
  | Some struct_ ->
    (match Record.LMap.find_opt label struct_.fields with
    | None -> None
    | Some row_element -> Some row_element.associated_type)


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
  | E_record r ->
    let lst = Record.LMap.to_kv_list_rev r in
    (match lst with
    | [ (Label "O", a); (Label "1", b) ] | [ (Label "1", b); (Label "0", a) ] ->
      Some (a, b)
    | _ -> None)
  | _ -> None


let extract_record : expression -> (Label.t * expression) list option =
 fun e ->
  match e.expression_content with
  | E_record lst -> Some (Record.LMap.to_kv_list lst)
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


let t_michelson_sum ?loc l l_ann r r_ann =
  let row decl_pos associated_type michelson_annotation : _ Rows.row_element_mini_c =
    { associated_type; decl_pos; michelson_annotation = Some michelson_annotation }
  in
  t_sum
    ?loc
    (Record.of_list [ Label "M_left", row 0 l l_ann; Label "M_right", row 1 r r_ann ])


let t_michelson_pair ?loc l l_ann r r_ann =
  let row decl_pos associated_type michelson_annotation : _ Rows.row_element_mini_c =
    { associated_type; decl_pos; michelson_annotation = Some michelson_annotation }
  in
  t_record ?loc (Record.of_list [ Label "0", row 0 l l_ann; Label "1", row 1 r r_ann ])

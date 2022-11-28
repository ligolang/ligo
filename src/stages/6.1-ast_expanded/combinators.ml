open Types
open Simple_utils
[@@@warning "-32"]

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

open Ligo_prim
open Literal_types

let t_constant ?loc injection parameters : type_expression =
  t_constant ?loc {language=Backend.Michelson.name; injection  ; parameters} ()
let get_t_inj (t:type_expression) (v:Ligo_prim.Literal_types.t) : (type_expression list) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters} when Ligo_prim.Literal_types.equal injection v -> Some parameters
  | _ -> None
let get_t_unary_inj (t:type_expression) (v:Ligo_prim.Literal_types.t) : type_expression option =
  match get_t_inj t v with
  | Some [a] -> Some a
  | _ -> None
let get_t_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_constant {language=_;injection; parameters = [k;v]} when Ligo_prim.Literal_types.equal injection Ligo_prim.Literal_types.Map -> Some (k,v)
  | _ -> None
let t__type_ ?loc () : type_expression = t_constant ?loc _type_ []
[@@map (_type_, ("list", "set", "signature","chain_id", "string", "bytes", "key", "key_hash", "int", "address", "operation", "nat", "tez", "timestamp", "unit", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr", "never", "mutation", "pvss_key", "baker_hash", "chest_key", "chest"))]
let t__type_ ?loc t t' : type_expression = t_constant ?loc _type_ [t; t']
[@@map (_type_, ("map"))]
let get_t__type_ (t : type_expression) : type_expression option = get_t_unary_inj t _type_
[@@map (_type_, ("list", "set", "signature","chain_id", "string", "bytes", "key", "key_hash", "int", "address", "operation", "nat", "tez", "timestamp", "unit", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr", "never", "mutation", "pvss_key", "baker_hash", "chest_key", "chest"))]
let is_t__type_ t =
  Option.is_some (get_t__type_ t)
[@@map (_type_, ("map", "list", "set", "signature","chain_id", "string", "bytes", "key", "key_hash", "int", "address", "operation", "nat", "tez", "timestamp", "unit", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr", "never", "mutation", "pvss_key", "baker_hash", "chest_key", "chest"))]
let get_t__type__exn t =
  match get_t__type_ t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))
[@@map (_type_, ("map", "list", "set", "signature","chain_id", "string", "bytes", "key", "key_hash", "int", "address", "operation", "nat", "tez", "timestamp", "unit", "bls12_381_g1", "bls12_381_g2", "bls12_381_fr", "never", "mutation", "pvss_key", "baker_hash", "chest_key", "chest"))]

let t_arrow param result ?loc ?source_type () : type_expression = t_arrow ?loc ?source_type {type1=param; type2=result} ()

let t_record ?loc ~layout fields  : type_expression = make_t ?loc (T_record {fields;layout})
let make_t_ez_record ?loc ?(layout=Layout.default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label.of_string x, ({associated_type=y;michelson_annotation=None;decl_pos=i} : row_element)) ) lst in
  let map = Record.of_list lst in
  t_record ?loc ~layout map

let get_a_string (t:expression) =
  match t.expression_content with
  | E_literal (Literal_string s) -> Some (Ligo_string.extract s)
  | _ -> None

let e_a_variable v ty = e_variable v ty

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

let e_a_let_mut_in ~loc x =
  let exp = e_let_mut_in x (get_type x.let_result) in
  { exp with location = loc }
let e_a_let_in ~loc x =
  let exp = e_let_in x (get_type x.let_result) in
  { exp with location = loc }

let get_sum_type (t : type_expression) (label : Label.t) : type_expression =
  match get_t_sum t with
  | None -> failwith "pattern expanded: could not get sum type"
  | Some struct_ ->
    match Record.LMap.find_opt label struct_.fields with
    | None -> failwith "pattern expanded: could not get row from its label"
    | Some row_element -> row_element.associated_type

let t_sum ?loc ~layout fields : type_expression = make_t ?loc (T_sum {fields;layout})
let t_sum_ez ?loc ?(layout=Layout.default_layout) (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi ~f:(fun i (x,y) -> (Label.of_string x, ({associated_type=y;michelson_annotation=None;decl_pos=i}:row_element)) ) lst in
  let map = Record.of_list lst in
  t_sum ?loc ~layout map
let t_bool ?loc ()       : type_expression = t_sum_ez ?loc
  [("True", t_unit ());("False", t_unit ())]
let get_t_bool (t:type_expression) : unit option = match t.type_content with
  | t when (compare_type_content t (t_bool ()).type_content) = 0 -> Some ()
  | _ -> None

let t_option ?loc typ : type_expression =
  t_sum_ez ?loc [
    ("Some", typ) ;
    ("None", t_unit ());
  ]

let t_record ?loc ~layout fields  : type_expression = make_t ?loc (T_record {fields;layout})
let ez_t_record ?loc ?(layout=Layout.default_layout) lst : type_expression =
  let m = Record.of_list lst in
  t_record ?loc ~layout m
let t_pair ?loc a b : type_expression =
  ez_t_record ?loc [
    (Label.of_int 0,{associated_type=a;michelson_annotation=None ; decl_pos = 0}) ;
    (Label.of_int 1,{associated_type=b;michelson_annotation=None ; decl_pos = 1}) ]
let is_michelson_or (t: _ Record.t) =
  let s = List.sort ~compare:(fun (k1, _) (k2, _) -> Label.compare k1 k2) @@
    Record.LMap.to_kv_list t in
  match s with
  | [ (Label "M_left", ta) ; (Label "M_right", tb) ] -> Some (ta,tb)
  | _ -> None

let is_michelson_pair (t: row_element Record.t) : (row_element * row_element) option =
  match Record.LMap.to_list t with
  | [ a ; b ] -> (
      if List.for_all ~f:(fun i -> Record.LMap.mem i t) @@ (Label.range 0 2)
      && Option.(
        is_some a.michelson_annotation || is_some b.michelson_annotation
      )
      then Some (a , b)
      else None
    )
  | _ -> None

let kv_list_of_t_sum ?(layout = Layout.default_layout) (m: row_element Record.t) =
  let lst = Record.LMap.to_kv_list m in
  match layout with
  | L_tree -> lst
  | L_comb -> (
      let aux (_ , ({ associated_type = _ ; decl_pos = a ; _ }: row_element)) (_ , ({ associated_type = _ ; decl_pos = b ; _ } : row_element)) = Int.compare a b in
      List.sort ~compare:aux lst
    )

let kv_list_of_t_record_or_tuple ?(layout = Layout.default_layout) (m: row_element Record.t) =
  let lst =
    if (Record.is_tuple m)
    then Record.tuple_of_record m
    else Record.LMap.to_kv_list m
  in
  match layout with
  | L_tree -> lst
  | L_comb -> (
      let aux (_ , ({ associated_type = _ ; decl_pos = a ; _ }: row_element)) (_ , ({ associated_type = _ ; decl_pos = b ; _ } : row_element)) = Int.compare a b in
      List.sort ~compare:aux lst
    )

let kv_list_of_record_or_tuple ~layout record_t_content record =
  let exps =
    if (Record.is_tuple record)
    then Record.tuple_of_record record
    else Record.LMap.to_kv_list record
  in
  match (layout : Layout.t) with
  | L_tree -> List.map ~f:snd exps
  | L_comb -> (
    let types = if (Record.is_tuple record)
                then Record.tuple_of_record record_t_content
                else Record.LMap.to_kv_list record_t_content in
    let te = List.map ~f:(fun ((label_t,t),(label_e,e)) ->
      assert (Label.equal label_t label_e) ; (*TODO TEST*)
      (t,e)) (List.zip_exn types exps) in
    let s = List.sort ~compare:(fun (({ associated_type = _ ; decl_pos = a ; _ }: row_element),_) ({ associated_type = _ ; decl_pos = b ; _ },_) -> Int.compare a b) te in
    List.map ~f:snd s
  )

let e_unit () : expression_content = E_literal (Literal_unit)

let get_e_tuple = fun t ->
  match t with
  | E_record r -> Some (List.map ~f:snd @@ Record.tuple_of_record r)
  | _ -> None

let e_bool b : expression_content =
  if b then
    E_constructor { constructor = (Label "True") ; element = (make_e (e_unit ()) (t_unit ())) }
  else
    E_constructor { constructor = (Label "False") ; element = (make_e (e_unit ()) (t_unit ())) }
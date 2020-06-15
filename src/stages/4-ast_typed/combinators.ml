open Types

let make_t ?(loc = Location.generated) type_content core = {type_content; location=loc; type_meta = core}
let make_e ?(location = Location.generated) expression_content type_expression = {
  expression_content ;
  type_expression ;
  location ;
}
let make_n_t type_name type_value = { type_name ; type_value }

let t_signature ?loc ?s ()  : type_expression = make_t ?loc (T_constant TC_signature) s
let t_chain_id ?loc ?s ()   : type_expression = make_t ?loc (T_constant TC_chain_id) s
let t_string ?loc ?s ()     : type_expression = make_t ?loc (T_constant TC_string) s
let t_bytes ?loc ?s ()      : type_expression = make_t ?loc (T_constant TC_bytes) s
let t_key ?loc ?s ()        : type_expression = make_t ?loc (T_constant TC_key) s
let t_key_hash ?loc ?s ()   : type_expression = make_t ?loc (T_constant TC_key_hash) s
let t_int ?loc ?s ()        : type_expression = make_t ?loc (T_constant TC_int) s
let t_address ?loc ?s ()    : type_expression = make_t ?loc (T_constant TC_address) s
let t_operation ?loc ?s ()  : type_expression = make_t ?loc (T_constant TC_operation) s
let t_nat ?loc ?s ()        : type_expression = make_t ?loc (T_constant TC_nat) s
let t_mutez ?loc ?s ()      : type_expression = make_t ?loc (T_constant TC_mutez) s
let t_timestamp ?loc ?s ()  : type_expression = make_t ?loc (T_constant TC_timestamp) s
let t_unit ?loc ?s ()       : type_expression = make_t ?loc (T_constant TC_unit) s
let t_option o ?loc ?s ()   : type_expression = make_t ?loc (T_operator (TC_option o)) s
let t_variable t ?loc ?s () : type_expression = make_t ?loc (T_variable t) s
let t_list t ?loc ?s ()     : type_expression = make_t ?loc (T_operator (TC_list t)) s
let t_set t ?loc ?s ()      : type_expression = make_t ?loc (T_operator (TC_set t)) s
let t_contract t ?loc ?s () : type_expression = make_t ?loc (T_operator (TC_contract t)) s


let t_record m ?loc ?s () : type_expression = make_t ?loc (T_record m) s
let make_t_ez_record ?loc (lst:(string * type_expression) list) : type_expression =
  let lst = List.mapi (fun i (x,y) -> (Label x, {field_type=y;michelson_annotation=None;field_decl_pos=i}) ) lst in
  let map = LMap.of_list lst in
  make_t ?loc (T_record map) None
let ez_t_record lst ?loc ?s () : type_expression =
  let m = LMap.of_list lst in
  t_record m ?loc ?s ()
let t_pair a b ?loc ?s () : type_expression =
  ez_t_record [
    (Label "0",{field_type=a;michelson_annotation=None ; field_decl_pos = 0}) ;
    (Label "1",{field_type=b;michelson_annotation=None ; field_decl_pos = 0}) ] ?loc ?s ()

let t_map ?loc k v ?s () = make_t ?loc (T_operator (TC_map { k ; v })) s
let t_big_map ?loc k v ?s () = make_t ?loc (T_operator (TC_big_map { k ; v })) s
let t_map_or_big_map ?loc k v ?s () = make_t ?loc (T_operator (TC_map_or_big_map { k ; v })) s

let t_sum m ?loc ?s () : type_expression = make_t ?loc (T_sum m) s
let make_t_ez_sum ?loc ?s (lst:(constructor' * ctor_content) list) : type_expression =
  let aux prev (k, v) = CMap.add k v prev in
  let map = List.fold_left aux CMap.empty lst in
  make_t ?loc (T_sum map) s

let t_bool ?loc ?s ()       : type_expression = make_t_ez_sum ?loc ?s
  [(Constructor "true", {ctor_type=t_unit ();michelson_annotation=None;ctor_decl_pos=0});(Constructor "false", {ctor_type=t_unit ();michelson_annotation=None;ctor_decl_pos=1})]

let t_function param result ?loc ?s () : type_expression = make_t ?loc (T_arrow {type1=param; type2=result}) s
let t_shallow_closure param result ?loc ?s () : type_expression = make_t ?loc (T_arrow {type1=param; type2=result}) s

let get_type_expression (x:expression) = x.type_expression
let get_type' (x:type_expression) = x.type_content
let get_expression (x:expression) = x.expression_content

let get_lambda e : lambda option = match e.expression_content with
  | E_lambda l -> Some l
  | _ -> None

let get_lambda_with_type e =
  match (e.expression_content , e.type_expression.type_content) with
  | E_lambda l , T_arrow {type1;type2} -> Some (l , (type1,type2))
  | _ -> None

let get_t_bool (t:type_expression) : unit option = match t.type_content with
  | T_variable v when Var.equal v Stage_common.Constant.t_bool -> Some ()
  | t when (compare t (t_bool ()).type_content) = 0-> Some ()
  | _ -> None

let get_t_int (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_int) -> Some ()
  | _ -> None

let get_t_nat (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_nat) -> Some ()
  | _ -> None

let get_t_unit (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_unit) -> Some ()
  | _ -> None

let get_t_mutez (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_mutez) -> Some ()
  | _ -> None

let get_t_bytes (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_bytes) -> Some ()
  | _ -> None

let get_t_string (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_string) -> Some ()
  | _ -> None

let get_t_contract (t:type_expression) : type_expression option = match t.type_content with
  | T_operator (TC_contract x) -> Some x
  | _ -> None

let get_t_option (t:type_expression) : type_expression option = match t.type_content with
  | T_operator (TC_option o) -> Some o
  | _ -> None

let get_t_list (t:type_expression) : type_expression option = match t.type_content with
  | T_operator (TC_list l) -> Some l
  | _ -> None

let get_t_set (t:type_expression) : type_expression option = match t.type_content with
  | T_operator (TC_set s) -> Some s
  | _ -> None 

let get_t_key (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_key) -> Some ()
  | _ -> None 

let get_t_signature (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_signature) -> Some ()
  | _ -> None

let get_t_key_hash (t:type_expression) : unit option = match t.type_content with
  | T_constant (TC_key_hash) -> Some ()
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i = 
    let opt = LMap.find_opt (Label (string_of_int i)) m in
    Option.bind (fun opt -> Some (opt,i+1)) opt
  in
  let l = Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux in
  List.map (fun {field_type;_} -> field_type) l


let get_t_tuple (t:type_expression) : type_expression list option = match t.type_content with
  | T_record lst -> Some (tuple_of_record lst)
  | _ -> None

let get_t_pair (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_record m ->
      let lst = tuple_of_record m in
      ( match List.(length lst = 2) with
        | true -> Some (List.(nth lst 0 , nth lst 1))
        | false -> None
      )
  | _ -> None

let get_t_function (t:type_expression) : (type_expression * type_expression) option = match t.type_content with
  | T_arrow {type1;type2} -> Some (type1,type2)
  | _ -> None

let get_t_function_exn t = match get_t_function t with
  | Some x -> x
  | None -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_sum (t:type_expression) : ctor_content constructor_map option = match t.type_content with
  | T_sum m -> Some m
  | _ -> None

let get_t_sum_exn (t:type_expression) : ctor_content constructor_map = match t.type_content with
  | T_sum m -> m
  | _ -> raise (Failure ("Internal error: broken invariant at " ^ __LOC__))

let get_t_record (t:type_expression) : field_content label_map option = match t.type_content with
  | T_record m -> Some m
  | _ -> None

let get_t_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_operator (TC_map { k ; v }) -> Some (k, v)
  | T_operator (TC_map_or_big_map { k ; v }) -> Some (k, v)
  | _ -> None

let get_t_big_map (t:type_expression) : (type_expression * type_expression) option =
  match t.type_content with
  | T_operator (TC_big_map { k ; v }) -> Some (k, v)
  | T_operator (TC_map_or_big_map { k ; v }) -> Some (k, v)
  | _ -> None

let get_t_map_key : type_expression -> type_expression option = fun t ->
  match get_t_map t with
  | Some (key,_) -> Some key
  | None -> None

let get_t_map_value : type_expression -> type_expression option = fun t ->
  match get_t_map t with
  | Some (_,value) -> Some value
  | None -> None

let get_t_big_map_key : type_expression -> type_expression option = fun t ->
  match get_t_big_map t with
  | Some (key,_) -> Some key
  | None -> None

let get_t_big_map_value : type_expression -> type_expression option = fun t ->
  match get_t_big_map t with
  | Some (_,value) -> Some value
  | None -> None

let is_t_map t = Option.is_some (get_t_map t)
let is_t_big_map t = Option.is_some (get_t_big_map t)

let assert_t_mutez : type_expression -> unit option = get_t_mutez
let assert_t_key = get_t_key
let assert_t_signature = get_t_signature
let assert_t_key_hash = get_t_key_hash
let assert_t_bytes = get_t_bytes
let assert_t_string = get_t_string

let assert_t_contract (t:type_expression) : unit option = match t.type_content with
  | T_operator (TC_contract _) -> Some ()
  | _ -> None

let is_t_list t = Option.is_some (get_t_list t)
let is_t_set t = Option.is_some (get_t_set t)
let is_t_nat t = Option.is_some (get_t_nat t)
let is_t_string t = Option.is_some (get_t_string t)
let is_t_bytes t = Option.is_some (get_t_bytes t)
let is_t_int t = Option.is_some (get_t_int t)

let assert_t_list_operation (t : type_expression) : unit option =
  match get_t_list t with
  | Some t' -> (
    match t'.type_content with
    | T_constant (TC_operation) -> Some ()
    | _ -> None
  )
  | None -> None

let assert_t_int : type_expression -> unit option = fun t -> match t.type_content with
  | T_constant (TC_int) -> Some ()
  | _ -> None

let assert_t_nat : type_expression -> unit option = fun t -> match t.type_content with
  | T_constant (TC_nat) -> Some ()
  | _ -> None

let assert_t_bool : type_expression -> unit option = fun v -> get_t_bool v
let assert_t_unit : type_expression -> unit option = fun v -> get_t_unit v

let e_record map : expression_content = E_record map
let ez_e_record (lst : (label * expression) list) : expression_content =
  let aux prev (k, v) = LMap.add k v prev in
  let map = List.fold_left aux LMap.empty lst in
  e_record map
let e_some s : expression_content = E_constant {cons_name=C_SOME;arguments=[s]}
let e_none (): expression_content = E_constant {cons_name=C_NONE; arguments=[]}

let e_unit () : expression_content =     E_literal (Literal_unit)
let e_int n : expression_content = E_literal (Literal_int n)
let e_nat n : expression_content = E_literal (Literal_nat n)
let e_mutez n : expression_content = E_literal (Literal_mutez n)
let e_string s : expression_content = E_literal (Literal_string s)
let e_bytes s : expression_content = E_literal (Literal_bytes s)
let e_timestamp s : expression_content = E_literal (Literal_timestamp s)
let e_address s : expression_content = E_literal (Literal_address s)
let e_signature s : expression_content = E_literal (Literal_signature s)
let e_key s : expression_content = E_literal (Literal_key s)
let e_key_hash s : expression_content = E_literal (Literal_key_hash s)
let e_chain_id s : expression_content = E_literal (Literal_chain_id s)
let e_operation s : expression_content = E_literal (Literal_operation s)
let e_lambda l : expression_content = E_lambda l
let e_pair a b : expression_content = ez_e_record [(Label "0",a);(Label "1", b)]
let e_application lamb args : expression_content = E_application {lamb;args}
let e_variable v : expression_content = E_variable v
let e_let_in let_binder inline rhs let_result = E_let_in { let_binder ; rhs ; let_result; inline }

let e_constructor constructor element: expression_content = E_constructor {constructor;element}

let e_bool b : expression_content = e_constructor (Constructor (string_of_bool b)) (make_e (e_unit ())(t_unit()))

let e_a_unit = make_e (e_unit ()) (t_unit ())
let e_a_int n = make_e (e_int n) (t_int ())
let e_a_nat n = make_e (e_nat n) (t_nat ())
let e_a_mutez n = make_e (e_mutez n) (t_mutez ())
let e_a_bool b = make_e (e_bool b) (t_bool ())
let e_a_string s = make_e (e_string s) (t_string ())
let e_a_address s = make_e (e_address s) (t_address ())
let e_a_pair a b = make_e (e_pair a b)
  (t_pair a.type_expression b.type_expression () )
let e_a_some s = make_e (e_some s) (t_option s.type_expression ())
let e_a_lambda l in_ty out_ty = make_e (e_lambda l) (t_function in_ty out_ty ())
let e_a_none t = make_e (e_none ()) (t_option t ())
let e_a_record r = make_e (e_record r) (t_record
  (LMap.map
    (fun t ->
      let field_type = get_type_expression t in
      {field_type ; michelson_annotation=None ; field_decl_pos = 0} )
    r ) () )
let e_a_application a b = make_e (e_application a b) (get_type_expression b)
let e_a_variable v ty = make_e (e_variable v) ty
let ez_e_a_record r = make_e (ez_e_record r) (ez_t_record (List.mapi (fun i (x, y) -> x, {field_type = y.type_expression ; michelson_annotation = None ; field_decl_pos = i}) r) ())
let e_a_let_in binder expr body attributes = make_e (e_let_in binder expr body attributes) (get_type_expression body)



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
  | E_constructor {constructor=Constructor name;element}
    when (String.equal name "true" || String.equal name "false") 
    && element.expression_content = e_unit () -> 
      Some (bool_of_string name)
  | _ -> None


let get_a_record_accessor = fun t ->
  match t.expression_content with
  | E_record_accessor {record; path} -> Some (record, path)
  | _ -> None

let get_declaration_by_name : program -> string -> declaration option = fun p name ->
  let aux : declaration -> bool = fun declaration ->
    match declaration with
    | Declaration_constant { binder ; expr=_ ; inline=_ } -> binder = Var.of_name name
    | Declaration_type _ -> false
  in
  List.find_opt aux @@ List.map Location.unwrap p

open Types

type ('a, 'b, 'c, 'd, 'e) expression_content_ =
  [%import: ('a, 'b, 'c, 'd, 'e) Types.expression_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_e", fun ~loc content : expr -> { fp = Location.wrap ~loc content })
        ; ("get_e", fun (x : Types.expr) -> Location.unwrap x.fp)
        ; ("get_e_loc", fun (x : Types.expr) -> Location.get_location x.fp)
        ]
    ; wrap_constructor = ("expression_content_", fun ~loc content -> make_e ~loc content)
    ; wrap_get = "expression_content_", get_e
    }]

type ('a, 'b) pattern_content_ = [%import: ('a, 'b) Types.pattern_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_p", fun ~loc content : pattern -> { fp = Location.wrap ~loc content })
        ; ("get_p", fun (x : Types.pattern) -> Location.unwrap x.fp)
        ; ("get_p_loc", fun (x : Types.pattern) -> Location.get_location x.fp)
        ]
    ; wrap_constructor = ("pattern_content_", fun ~loc content -> make_p ~loc content)
    ; wrap_get = "pattern_content_", get_p
    }]

type 'a type_expression_content_ = [%import: 'a Types.type_expression_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_t", fun ~loc content : ty_expr -> { fp = Location.wrap ~loc content })
        ; ("get_t", fun (x : Types.ty_expr) -> Location.unwrap x.fp)
        ; ("get_t_loc", fun (x : Types.ty_expr) -> Location.get_location x.fp)
        ]
    ; wrap_constructor =
        ("type_expression_content_", fun ~loc content -> make_t ~loc content)
    ; wrap_get = "type_expression_content_", get_t
    }]

type ('a, 'b, 'c, 'd, 'e) declaration_content_ =
  [%import: ('a, 'b, 'c, 'd, 'e) Types.declaration_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_d", fun ~loc content : declaration -> { fp = Location.wrap ~loc content })
        ; ("get_d", fun (x : Types.declaration) -> Location.unwrap x.fp)
        ; ("get_d_loc", fun (x : Types.declaration) -> Location.get_location x.fp)
        ]
    ; wrap_constructor =
        ( "declaration_content_"
        , fun ~loc declaration_content -> make_d ~loc declaration_content )
    ; wrap_get = "declaration_content_", get_d
    }]

type ('a, 'b, 'c) statement_content_ = [%import: ('a, 'b, 'c) Types.statement_content_]
[@@deriving
  ez
    { prefixes =
        [ ( "make_s"
          , fun ~loc content : statement ->
              { fp = (Location.wrap ~loc content : ('a, 'b, 'c) Types.statement_) } )
        ; ("get_s", fun (x : Types.statement) -> Location.unwrap x.fp)
        ; ("get_s_loc", fun (x : Types.statement) -> Location.get_location x.fp)
        ]
    ; wrap_constructor =
        ("statement_content_", fun ~loc statement_content -> make_s ~loc statement_content)
    ; wrap_get = "statement_content_", get_s
    }]

type ('a, 'b) block_ = [%import: ('a, 'b) Types.block_]
[@@deriving
  ez
    { prefixes =
        [ ("make_b", fun ~loc content : block -> { fp = Location.wrap ~loc content })
        ; ("get_b", fun (x : Types.block) -> Location.unwrap x.fp)
        ; ("get_b_loc", fun (x : Types.block) -> Location.get_location x.fp)
        ]
    }]

type ('a, 'b) mod_expr_content_ = [%import: ('a, 'b) Types.mod_expr_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_m", fun ~loc content : mod_expr -> { fp = Location.wrap ~loc content })
        ; ("get_m", fun (x : Types.mod_expr) -> Location.unwrap x.fp)
        ]
    ; wrap_constructor =
        ("mod_expr_content_", fun module_content ~loc -> make_m ~loc module_content)
    ; wrap_get = "mod_expr_content_", get_m
    }]

type ('a, 'b, 'c, 'd, 'e) instruction_content_ =
  [%import: ('a, 'b, 'c, 'd, 'e) Types.instruction_content_]
[@@deriving
  ez
    { prefixes =
        [ ("make_i", fun ~loc content : instruction -> { fp = Location.wrap ~loc content })
        ; ("get_i", fun (x : Types.instruction) -> Location.unwrap x.fp)
        ; ("get_i_loc", fun (x : Types.instruction) -> Location.get_location x.fp)
        ]
    ; wrap_constructor =
        ( "instruction_content_"
        , fun instruction_content ~loc -> make_i ~loc instruction_content )
    ; wrap_get = "instruction_content_", get_i
    }]

type ('a, 'b, 'c) program_entry_ = [%import: ('a, 'b, 'c) Types.program_entry_]
[@@deriving
  ez
    { prefixes =
        [ ("make_pe", fun content : program_entry -> { fp = content })
        ; ("get_pe", fun (x : Types.program_entry) -> x.fp)
        ]
    ; wrap_constructor = ("program_entry_", fun c -> make_pe c)
    ; wrap_get = "program_entry_", get_pe
    }]

type ('a, 'b) program_ = [%import: ('a, 'b) Types.program_]
[@@deriving
  ez
    { prefixes =
        [ ("make_prg", fun content : program -> { fp = content })
        ; ("get_prg", fun (x : Types.program) -> x.fp)
        ]
    }]

let e_literal ~loc l : expr = make_e ~loc @@ E_literal l

let e__type_ ~loc p : expr = make_e ~loc @@ E_literal (Literal__type_ p)
  [@@map
    _type_
    , ( "address"
      , "bytes"
      , "signature"
      , "key"
      , "key_hash"
      , "chain_id"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr" )]


let e__type__z ~loc n : expr = make_e ~loc @@ E_literal (Literal__type_ n)
  [@@map _type_, ("int", "nat", "timestamp", "mutez")]


let e__type_ ~loc n : expr = e__type__z ~loc @@ Z.of_int n
  [@@map _type_, ("int", "nat", "timestamp", "mutez")]


let e_false ~loc = e_constant ~loc { cons_name = C_FALSE; arguments = [] }
let e_true ~loc = e_constant ~loc { cons_name = C_TRUE; arguments = [] }

let e_string ~loc str : expr =
  make_e ~loc @@ E_literal (Literal_string (Simple_utils.Ligo_string.standard str))


let e_unit ~loc : expr = make_e ~loc @@ E_literal Literal_unit

let is_e_unit (e : expr) : bool =
  match Location.unwrap e.fp with
  | E_literal Literal_unit -> true
  | _ -> false


let e_bytes_raw ~loc (b : bytes) : expr = make_e ~loc @@ E_literal (Literal_bytes b)
let e_bytes_hex ~loc b : expr = e_bytes_raw ~loc @@ Hex.to_bytes b
let e_bytes_hex_ez ~loc b : expr = e_bytes_raw ~loc @@ Hex.to_bytes (`Hex b)
let e_bytes_string ~loc (s : string) : expr = e_bytes_hex ~loc @@ Hex.of_string s

let simpl_var_decl ~loc v let_rhs =
  s_decl
    ~loc
    (d_var ~loc { type_params = None; pattern = p_var ~loc v; rhs_type = None; let_rhs })


let simpl_const_decl ~loc v let_rhs =
  s_decl
    ~loc
    (d_const
       ~loc
       { type_params = None; pattern = p_var ~loc v; rhs_type = None; let_rhs })


(* might generate those uh ? *)
include struct
  let e_map_find_opt ~loc k map =
    e_constant ~loc { cons_name = C_MAP_FIND_OPT; arguments = [ k; map ] }


  let e_map_add ~loc k v old =
    e_constant ~loc { cons_name = C_MAP_ADD; arguments = [ k; v; old ] }


  let e_set_remove ~loc ele set =
    e_constant ~loc { cons_name = C_SET_REMOVE; arguments = [ ele; set ] }


  let e_map_remove ~loc ele map =
    e_constant ~loc { cons_name = C_MAP_REMOVE; arguments = [ ele; map ] }


  let e_set_add ~loc ele set =
    e_constant ~loc { cons_name = C_SET_ADD; arguments = [ ele; set ] }
end

let e_unopt ~loc matchee none_body (var_some, some_body) =
  let some_case =
    let pattern = p_variant ~loc (Label "Some") (Some (p_var ~loc var_some)) in
    Case.{ pattern; rhs = some_body }
  in
  let none_case =
    let pattern = p_variant ~loc (Label "None") None in
    Case.{ pattern; rhs = none_body }
  in
  e_match ~loc { expr = matchee; cases = some_case, [ none_case ] }


let let_unit_in rhs body =
  e_let_in
    ~loc:(Location.cover (get_e_loc rhs) (get_e_loc body))
    { is_rec = false
    ; type_params = None
    ; lhs = List.Ne.singleton @@ p_unit ~loc:Location.generated
    ; rhs_type = None
    ; rhs
    ; body
    }


let let_ignore_in rhs body =
  e_let_in
    ~loc:(Location.cover (get_e_loc rhs) (get_e_loc body))
    { is_rec = false
    ; type_params = None
    ; lhs =
        List.Ne.singleton
        @@ p_var
             ~loc:Location.generated
             (Variable.fresh ~name:"_" ~loc:Location.generated ())
    ; rhs_type = None
    ; rhs
    ; body
    }


let block_of_statements stmts =
  let loc =
    List.Ne.fold_left
      ~init:Location.generated
      ~f:Location.cover
      (List.Ne.map get_s_loc stmts)
  in
  make_b ~loc stmts


let e_pair ~loc l r = e_tuple ~loc (Simple_utils.List.Ne.of_list [ l; r ])

let e_record_ez ~loc (lst : (string * expr) list) =
  e_record_pun ~loc
  @@ List.map lst ~f:(fun (l, e) -> Field.Complete (Label.of_string l, e))


let e_lambda_ez ~loc var ?ascr ?mut_flag output_type result : expr =
  e_lambda ~loc { binder = Ligo_prim.Param.make ?mut_flag var ascr; output_type; result }


let e_variable_ez ~loc str = e_variable ~loc (Variable.of_input_var ~loc str)

let tv__type_ ~loc () : ty_expr =
  let open Ligo_prim.Literal_types in
  t_var ~loc (v__type_ ~loc)
  [@@map
    _type_
    , ( "string"
      , "bytes"
      , "int"
      , "operation"
      , "nat"
      , "tez"
      , "unit"
      , "address"
      , "signature"
      , "key"
      , "key_hash"
      , "timestamp"
      , "bls12_381_g1"
      , "bls12_381_g2"
      , "bls12_381_fr"
      , "chain_id"
      , "set"
      , "contract"
      , "option"
      , "list"
      , "map" )]


let t__type_ ~loc t : ty_expr =
  t_app
    ~loc
    { constr = tv__type_ ~loc (); type_args = Simple_utils.List.Ne.of_list [ t ] }
  [@@map _type_, ("list", "set", "contract", "option")]


let t__type_ ~loc t1 t2 : ty_expr =
  t_app
    ~loc
    { constr = tv__type_ ~loc (); type_args = Simple_utils.List.Ne.of_list [ t1; t2 ] }
  [@@map _type_, "map"]


let e_none ~loc =
  e_applied_constructor ~loc { constructor = Label.of_string "None"; element = e_unit ~loc }


let e_some element = e_applied_constructor { constructor = Label.of_string "Some"; element }

let e_bool ~loc b =
  let e_true_ctor ~loc =
    e_applied_constructor ~loc { constructor = Label.of_string "True"; element = e_unit ~loc }
  in
  let e_false_ctor ~loc =
    e_applied_constructor ~loc { constructor = Label.of_string "False"; element = e_unit ~loc }
  in
  if b then e_true_ctor ~loc else e_false_ctor ~loc


let t_fun_of_list ~loc (lst : ty_expr list) =
  match List.rev lst with
  | [] -> assert false
  | hd :: tl -> List.fold tl ~init:hd ~f:(fun acc t -> t_fun ~loc (t, acc))


let e_type_abstract_ez ty_params init =
  let loc = get_e_loc init in
  List.Ne.fold_right ty_params ~init ~f:(fun type_binder result ->
      e_type_abstraction ~loc Type_abstraction.{ type_binder; result })


let t_type_forall_ez ty_params init =
  let loc = get_t_loc init in
  List.Ne.fold_right ty_params ~init ~f:(fun ty_binder type_ ->
      t_for_all ~loc Abstraction.{ ty_binder; kind = Type; type_ })


let get_pattern_binders (p : pattern) : Variable.t list =
  let fp : (Variable.t list, unit) pattern_ -> Variable.t list =
   fun p ->
    let init =
      match Location.unwrap p with
      | P_var x -> [ x ]
      | P_pun_record lst ->
        List.filter_map lst ~f:(function
            | Punned { wrap_content; location } ->
              Some (Variable.of_input_var ~loc:location (Label.to_string wrap_content))
            | _ -> None)
      | _ -> []
    in
    fold_pattern_ List.append (fun acc () -> acc) init p
  in
  let folder =
    Recursion_schemes.Catamorphism.
      { expr = (fun _ -> ())
      ; ty_expr = (fun _ -> ())
      ; pattern = fp
      ; statement = (fun _ -> ())
      ; block = (fun _ -> ())
      ; mod_expr = (fun _ -> ())
      ; instruction = (fun _ -> ())
      ; declaration = (fun _ -> ())
      ; program_entry = (fun _ -> ())
      ; program = (fun _ -> ())
      }
  in
  Recursion_schemes.Catamorphism.cata_pattern ~f:folder p

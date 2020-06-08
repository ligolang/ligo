[@@@warning "-30"]

open Types_utils

(* pseudo-typeclasses: interfaces that must be provided for arguments
   of the givent polymmorphic types. For now, only one typeclass can
   be specified for a given polymorphic type. The implementation is
   provided by the Comparable module *)
(*@ typeclass poly_unionfind comparable *)
(*@ typeclass poly_set       comparable *)

type type_constant =
    | TC_unit
    | TC_string
    | TC_bytes
    | TC_nat
    | TC_int
    | TC_mutez
    | TC_operation
    | TC_address
    | TC_key
    | TC_key_hash
    | TC_chain_id
    | TC_signature
    | TC_timestamp
    | TC_void

type te_cmap = ctor_content constructor_map
and te_lmap = field_content label_map
and type_meta = ast_core_type_expression option

and type_content =
  | T_sum of te_cmap
  | T_record of te_lmap
  | T_arrow of arrow
  | T_variable of type_variable
  | T_constant of type_constant
  | T_operator of type_operator

and arrow = {
    type1: type_expression;
    type2: type_expression;
  }

and annot_option = string option

and ctor_content = {
    ctor_type : type_expression;
    michelson_annotation : annot_option;
    ctor_decl_pos : int;
}

and field_content = {
    field_type : type_expression;
    michelson_annotation : annot_option;
    field_decl_pos : int;
}

and type_map_args = {
    k : type_expression;
    v : type_expression;
  }

and michelson_or_args = {
    l : type_expression;
    r : type_expression;
  }

and type_operator =
  | TC_contract of type_expression
  | TC_option of type_expression
  | TC_list of type_expression
  | TC_set of type_expression
  | TC_map of type_map_args
  | TC_big_map of type_map_args
  | TC_map_or_big_map of type_map_args

and type_expression = {
    type_content: type_content;
    type_meta: type_meta;
    location: location;
  }

type literal =
  | Literal_unit
  | Literal_int of z
  | Literal_nat of z
  | Literal_timestamp of z
  | Literal_mutez of z
  | Literal_string of ligo_string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_void
  | Literal_operation of packed_internal_operation


and matching_content_cons = {
    hd : expression_variable;
    tl : expression_variable;
    body : expression;
    tv : type_expression;
  }

and matching_content_list = {
    match_nil : expression ;
    match_cons : matching_content_cons;
  }

and matching_content_some = {
    opt  : expression_variable ;
    body : expression ;
    tv   : type_expression ;
  }

and matching_content_option = {
    match_none : expression ;
    match_some : matching_content_some ;
  }

and expression_variable_list = expression_variable list
and type_expression_list = type_expression list

and matching_content_case = {
    constructor : constructor' ;
    pattern : expression_variable ;
    body : expression ;
  }

and matching_content_case_list = matching_content_case list

and matching_content_variant = {
    cases: matching_content_case_list;
    tv: type_expression;
  }

and matching_expr =
  | Match_list    of matching_content_list
  | Match_option  of matching_content_option
  | Match_variant of matching_content_variant

and constant' =
  | C_INT
  | C_UNIT
  | C_NIL
  | C_NOW
  | C_IS_NAT
  | C_SOME
  | C_NONE
  | C_ASSERTION
  | C_ASSERT_INFERRED
  | C_FAILWITH
  | C_UPDATE
  (* Loops *)
  | C_ITER
  | C_FOLD_WHILE
  | C_FOLD_CONTINUE
  | C_FOLD_STOP
  | C_LOOP_LEFT
  | C_LOOP_CONTINUE
  | C_LOOP_STOP
  | C_FOLD
  (* MATH *)
  | C_NEG
  | C_ABS
  | C_ADD
  | C_SUB
  | C_MUL
  | C_EDIV
  | C_DIV
  | C_MOD
  (* LOGIC *)
  | C_NOT
  | C_AND
  | C_OR
  | C_XOR
  | C_LSL
  | C_LSR
  (* COMPARATOR *)
  | C_EQ
  | C_NEQ
  | C_LT
  | C_GT
  | C_LE
  | C_GE
  (* Bytes/ String *)
  | C_SIZE
  | C_CONCAT
  | C_SLICE
  | C_BYTES_PACK
  | C_BYTES_UNPACK
  | C_CONS
  (* Pair *)
  | C_PAIR
  | C_CAR
  | C_CDR
  | C_LEFT
  | C_RIGHT
  (* Set *)
  | C_SET_EMPTY
  | C_SET_LITERAL
  | C_SET_ADD
  | C_SET_REMOVE
  | C_SET_ITER
  | C_SET_FOLD
  | C_SET_MEM
  (* List *)
  | C_LIST_EMPTY
  | C_LIST_LITERAL
  | C_LIST_ITER
  | C_LIST_MAP
  | C_LIST_FOLD
  (* Maps *)
  | C_MAP
  | C_MAP_EMPTY
  | C_MAP_LITERAL
  | C_MAP_GET
  | C_MAP_GET_FORCE
  | C_MAP_ADD
  | C_MAP_REMOVE
  | C_MAP_UPDATE
  | C_MAP_ITER
  | C_MAP_MAP
  | C_MAP_FOLD
  | C_MAP_MEM
  | C_MAP_FIND
  | C_MAP_FIND_OPT
  (* Big Maps *)
  | C_BIG_MAP
  | C_BIG_MAP_EMPTY
  | C_BIG_MAP_LITERAL
  (* Crypto *)
  | C_SHA256
  | C_SHA512
  | C_BLAKE2b
  | C_HASH
  | C_HASH_KEY
  | C_CHECK_SIGNATURE
  | C_CHAIN_ID
  (* Blockchain *)
  | C_CALL
  | C_CONTRACT
  | C_CONTRACT_OPT
  | C_CONTRACT_ENTRYPOINT
  | C_CONTRACT_ENTRYPOINT_OPT
  | C_AMOUNT
  | C_BALANCE
  | C_SOURCE
  | C_SENDER
  | C_ADDRESS
  | C_SELF
  | C_SELF_ADDRESS
  | C_IMPLICIT_ACCOUNT
  | C_SET_DELEGATE
  | C_CREATE_CONTRACT
  | C_CONVERT_TO_LEFT_COMB
  | C_CONVERT_TO_RIGHT_COMB
  | C_CONVERT_FROM_LEFT_COMB
  | C_CONVERT_FROM_RIGHT_COMB

and declaration_loc = declaration location_wrap

and program = declaration_loc list

(* A Declaration_constant is described by
 *   a name + a type-annotated expression
 *   a boolean indicating whether it should be inlined
 *   the environment before the declaration (the original environment)
 *   the environment after the declaration (i.e. with that new declaration added to the original environment). *)
and declaration_constant = {
    binder : expression_variable ;
    expr : expression ;
    inline : bool ;
  }

and declaration_type = {
    type_binder : type_variable ;
    type_expr : type_expression ;
  }

and declaration =
  | Declaration_constant of declaration_constant
  | Declaration_type of declaration_type

and expression = {
    expression_content: expression_content ;
    location: location ;
    type_expression: type_expression ;
  }

and map_kv = {
    k : expression ;
    v : expression ;
  }

and look_up = {
    ds : expression;
    ind : expression;
  }

and expression_label_map = expression label_map
and map_kv_list = map_kv list
and expression_list = expression list

and expression_content =
  (* Base *)
  | E_literal of literal
  | E_constant of constant (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in of let_in
  (* Variant *)
  | E_constructor of constructor (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update

and constant = {
    cons_name: constant' ;
    arguments: expression_list ;
  }

and application = {
  lamb: expression ;
  args: expression ;
  }

and lambda =  {
    binder: expression_variable ;
    (* input_type: type_expression option ; *)
    (* output_type: type_expression option ; *)
    result: expression ;
  }

and let_in = {
    let_binder: expression_variable ;
    rhs: expression ;
    let_result: expression ;
    inline : bool ;
  }

and recursive = {
  fun_name : expression_variable;
  fun_type : type_expression;
  lambda : lambda;
}

and constructor = {
    constructor: constructor';
    element: expression ;
  }

and record_accessor = {
    record: expression ;
    path: label ;
  }

and record_update = {
    record: expression ;
    path: label ;
    update: expression ;
  }

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }

and ascription = {
    anno_expr: expression ;
    type_annotation: type_expression ;
  }

and environment_element_definition =
  | ED_binder
  | ED_declaration of environment_element_definition_declaration

and environment_element_definition_declaration = {
    expr: expression ;
    free_variables: free_variables ;
  }

and free_variables = expression_variable list

and environment_element = {
    type_value: type_expression ;
    source_environment: environment ;
    definition: environment_element_definition ;
  }

and expression_environment = environment_binding list

and environment_binding = {
    expr_var: expression_variable ;
    env_elt: environment_element ;
  }

and type_environment = type_environment_binding list

and type_environment_binding = {
    type_variable: type_variable ;
    type_: type_expression ;
}

and environment = {
  expression_environment: expression_environment ;
  type_environment: type_environment ;
}

and named_type_content = {
    type_name : type_variable;
    type_value : type_expression;
  }





(* Solver types *)

(* typevariable: to_string = (fun s -> Format.asprintf "%a" Var.pp s) *)
type unionfind = type_variable poly_unionfind

(* core *)

(* add information on the type or the kind for operator *)
type constant_tag =
  | C_arrow     (* * -> * -> *    isn't this wrong? *)
  | C_option    (* * -> * *)
  | C_record    (* ( label , * ) … -> * *)
  | C_variant   (* ( label , * ) … -> * *)
  | C_map       (* * -> * -> * *)
  | C_big_map   (* * -> * -> * *)
  | C_list      (* * -> * *)
  | C_set       (* * -> * *)
  | C_unit      (* * *)
  | C_string    (* * *)
  | C_nat       (* * *)
  | C_mutez     (* * *)
  | C_timestamp (* * *)
  | C_int       (* * *)
  | C_address   (* * *)
  | C_bytes     (* * *)
  | C_key_hash  (* * *)
  | C_key       (* * *)
  | C_signature (* * *)
  | C_operation (* * *)
  | C_contract  (* * -> * *)
  | C_chain_id  (* * *)

(* TODO: rename to type_expression or something similar (it includes variables, and unevaluated functions + applications *)
type type_value_ =
  | P_forall       of p_forall
  | P_variable     of type_variable
  | P_constant     of p_constant
  | P_apply        of p_apply
and type_value = {
  tsrc : string;
  t : type_value_ ;
}

and p_apply = {
    tf : type_value ;
    targ : type_value ;
}
and p_ctor_args = type_value list
and p_constant = {
    p_ctor_tag : constant_tag ;
    p_ctor_args : p_ctor_args ;
  }
and p_constraints = type_constraint list
and p_forall = {
  binder      : type_variable ;
  constraints : p_constraints ;
  body        : type_value ;
}

(* Different type of constraint *)
and ctor_args = type_variable list (* non-empty list *)
and simple_c_constructor = {
    ctor_tag : constant_tag ;
    ctor_args : ctor_args ;
  }
and simple_c_constant = {
    constant_tag: constant_tag ; (* for type constructors that do not take arguments *)
  }
and c_const = {
    c_const_tvar : type_variable ;
    c_const_tval : type_value ;
  }
and c_equation = {
  aval : type_value ;
  bval : type_value ;
}
and tc_args = type_value list
and c_typeclass = {
  tc_args : tc_args ;
  typeclass : typeclass ;
}
and c_access_label = {
    c_access_label_tval : type_value ;
    accessor : label ;
    c_access_label_tvar : type_variable ;
  }

and type_constraint = {
  reason : string ;
  c : type_constraint_ ;
}
and type_constraint_ =
  (* | C_assignment of (type_variable * type_pattern) *)
  | C_equation of c_equation (* TVA = TVB *)
  | C_typeclass of c_typeclass (* TVL ∈ TVLs, for now in extension, later add intensional (rule-based system for inclusion in the typeclass) *)
  | C_access_label of c_access_label (* poor man's type-level computation to ensure that TV.label is type_variable *)
(* | … *)

(* is the first list in case on of the type of the type class as a kind *->*->* ? *)
and tc_allowed = type_value list
and typeclass = tc_allowed list

(* end core *)

type c_constructor_simpl_typeVariableMap = c_constructor_simpl typeVariableMap
and constraints_typeVariableMap = constraints typeVariableMap
and type_constraint_simpl_list = type_constraint_simpl list
and structured_dbs = {
  all_constraints          : type_constraint_simpl_list ;
  aliases                  : unionfind ;
  (* assignments (passive data structure). *)
  (*   Now                 : just a map from unification vars to types (pb: what about partial types?) *)
  (*   maybe just local assignments (allow only vars as children of pair(α,β)) *)
  (* TODO                  : the rhs of the map should not repeat the variable name. *)
  assignments              : c_constructor_simpl_typeVariableMap ;
  grouped_by_variable      : constraints_typeVariableMap ; (* map from (unionfind) variables to constraints containing them *)
  cycle_detection_toposort : unit ;                        (* example of structured db that we'll add later *)
}

and c_constructor_simpl_list = c_constructor_simpl list
and c_poly_simpl_list        = c_poly_simpl        list
and c_typeclass_simpl_list   = c_typeclass_simpl   list
and constraints = {
  (* If implemented in a language with decent sets, these should be sets not lists. *)
  constructor : c_constructor_simpl_list ; (* List of ('a = constructor(args…)) constraints *)
  poly        : c_poly_simpl_list        ; (* List of ('a = forall 'b, some_type) constraints *)
  tc          : c_typeclass_simpl_list   ; (* List of (typeclass(args…)) constraints *)
}
and type_variable_list = type_variable list
and c_constructor_simpl = {
  reason_constr_simpl : string ;
  tv : type_variable;
  c_tag : constant_tag;
  tv_list : type_variable_list;
}
and c_const_e = {
    c_const_e_tv : type_variable ;
    c_const_e_te : type_expression ;
  }
and c_equation_e = {
    aex : type_expression ;
    bex : type_expression ;
  }
and c_typeclass_simpl = {
  reason_typeclass_simpl : string ;
  tc   : typeclass          ;
  args : type_variable_list ;
}
and c_poly_simpl = {
  reason_poly_simpl : string ;
  tv     : type_variable ;
  forall : p_forall      ;
}
and type_constraint_simpl =
  | SC_Constructor of c_constructor_simpl             (* α = ctor(β, …) *)
  | SC_Alias       of c_alias                         (* α = β *)
  | SC_Poly        of c_poly_simpl                    (* α = forall β, δ where δ can be a more complex type *)
  | SC_Typeclass   of c_typeclass_simpl               (* TC(α, …) *)

and c_alias = {
    reason_alias_simpl : string ;
    a : type_variable ;
    b : type_variable ;
  }


(* sub-sub component: lazy selector (don't re-try all selectors every time) *)
(* For now: just re-try everytime *)

(* selector / propagation rule for breaking down composite types *)
(* For now: break pair(a, b) = pair(c, d) into a = c, b = d *)
type output_break_ctor = {
    a_k_var : c_constructor_simpl ;
    a_k'_var' : c_constructor_simpl ;
  }

type output_specialize1 = {
    poly : c_poly_simpl ;
    a_k_var : c_constructor_simpl ;
  }

type m_break_ctor__already_selected = output_break_ctor poly_set
type m_specialize1__already_selected = output_specialize1 poly_set

type already_selected = {
  break_ctor  : m_break_ctor__already_selected  ;
  specialize1 : m_specialize1__already_selected ;
}

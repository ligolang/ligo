[@@@warning "-30"]

include Types_utils

type type_constant =
    | TC_unit
    | TC_string
    | TC_bytes
    | TC_nat
    | TC_int
    | TC_mutez
    | TC_bool
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
}

and field_content = {
    field_type : type_expression;
    michelson_annotation : annot_option;
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
  | TC_arrow of arrow


and type_expression = {
    type_content: type_content;
    type_meta: type_meta;
    location: location;
  }

type literal =
  | Literal_unit
  | Literal_bool of bool
  | Literal_int of int
  | Literal_nat of int
  | Literal_timestamp of int
  | Literal_mutez of int
  | Literal_string of string
  | Literal_bytes of bytes
  | Literal_address of string
  | Literal_signature of string
  | Literal_key of string
  | Literal_key_hash of string
  | Literal_chain_id of string
  | Literal_void
  | Literal_operation of packed_internal_operation

type matching_content_bool = {
    match_true : expression ;
    match_false : expression ;
  }

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

and matching_content_tuple = {
    vars : expression_variable_list ;
    body : expression ;
    tvs : type_expression_list ;
  }

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
  | Match_bool    of matching_content_bool
  | Match_list    of matching_content_list
  | Match_option  of matching_content_option
  | Match_tuple   of matching_content_tuple
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

and declaration_loc = declaration location_wrap

and program = declaration_loc list

and declaration_constant = {
    binder : expression_variable ;
    expr : expression ;
    inline : bool ;
    post_env : full_environment ;
  }

and declaration =
  (* A Declaration_constant is described by
   *   a name + a type-annotated expression
   *   a boolean indicating whether it should be inlined
   *   the environment before the declaration (the original environment)
   *   the environment after the declaration (i.e. with that new declaration added to the original environment). *)
  | Declaration_constant of declaration_constant
  (*
  | Declaration_type of (type_variable * type_expression)
  | Declaration_constant of (named_expression * (full_environment * full_environment))
  *)
(* | Macro_declaration of macro_declaration *)

and expression = {
    expression_content: expression_content ;
    location: location ;
    type_expression: type_expression ;
    environment: full_environment ;
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
    source_environment: full_environment ;
    definition: environment_element_definition ;
  }

and environment = environment_binding list

and environment_binding = {
    expr_var: expression_variable ;
    env_elt: environment_element ;
  }

and type_environment = type_environment_binding list

and type_environment_binding = {
    type_variable: type_variable ;
    type_: type_expression ;
}

(* SUBST ??? *)
and small_environment = {
  expression_environment: environment ;
  type_environment: type_environment ;
}

and full_environment = small_environment list_ne

and named_type_content = {
    type_name : type_variable;
    type_value : type_expression;
  }

[@@@warning "-30"]

open Types_utils

type string_option = string option

type attribute = {
  inline: bool ;
} 
type program_loc = declaration location_wrap
and program = program_loc list
and declaration_type = {
    type_binder : type_variable ;
    type_expr : type_expression ;
  }
and declaration_constant = {
    binder : expression_variable ;
    type_opt : type_expression_option ;
    inline : attribute ;
    expr : expression ;
  }
and declaration =
  | Declaration_type of declaration_type
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   a boolean indicating whether it should be inlined
   *   an expression *)
  | Declaration_constant of declaration_constant

(* | Macro_declaration of macro_declaration *)

and ctor_constructor_map = ctor_content constructor_map
and field_label_map = field_content label_map
and type_expression_list = type_expression list
and type_operator =
  | TC_contract
  | TC_option
  | TC_list
  | TC_set
  | TC_map
  | TC_big_map
  | TC_map_or_big_map
  | TC_michelson_pair
  | TC_michelson_or
  | TC_michelson_pair_right_comb
  | TC_michelson_pair_left_comb
  | TC_michelson_or_right_comb
  | TC_michelson_or_left_comb
and content_type_operator = {
    type_operator : type_operator ;
    arguments : type_expression_list ;
  }
and type_constant =
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
and type_content =
  | T_sum of ctor_constructor_map
  | T_record of field_label_map
  | T_arrow of arrow
  | T_variable of type_variable
  | T_constant of type_constant
  | T_operator of content_type_operator

and arrow = {
    type1: type_expression ;
    type2: type_expression ;
  }
and ctor_content = {
    ctor_type : type_expression ;
    michelson_annotation : string_option ;
    ctor_decl_pos : int ;
  }
and field_content = {
    field_type : type_expression ;
    field_annotation : string_option ;
    field_decl_pos : int ;
  }

and type_expression = {
  content  : type_content ;
  sugar    : sugar_type_expression_option ;
  location : location ;
  }

and expression = {
  content  : expression_content ;
  sugar    : sugar_expression_option ;
  location : location ;
  }

and expression_label_map = expression label_map
and literal =
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
  | Literal_operation of packed_internal_operation
and expression_content =
  | E_literal of literal
  | E_constant of constant
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in of let_in
  | E_raw_code of raw_code
  | E_constructor of constructor
  | E_matching of matching
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update
  | E_ascription of ascription

and expression_list = expression list
and constant = {
    cons_name: constant' ;
    arguments: expression_list ;
  }

and application = {
    lamb: expression ;
    args: expression ;
  }

and type_expression_option = type_expression option

and lambda = {
    binder: expression_variable ;
    input_type: type_expression_option ;
    output_type: type_expression_option ;
    result: expression ;
  }

and recursive = {
    fun_name : expression_variable ;
    fun_type : type_expression ;
    lambda : lambda ;
  }
 
and let_binder = {
    binder : expression_variable ;
    ascr : type_expression_option ;
  }
and let_in = {
    let_binder: let_binder ;
    rhs: expression ;
    let_result: expression ;
    inline: bool ;
  }

and raw_code = { 
  language : string ;
  code : expression ;
  }

and constructor = {
    constructor: constructor' ;
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
and match_cons = {
    hd : expression_variable ;
    tl : expression_variable ;
    body : expression ;
  }
and match_list = {
    match_nil  : expression ;
    match_cons : match_cons ;
  }
and match_some = {
    opt : expression_variable ;
    body : expression ;
  }
and match_option = {
    match_none : expression ;
    match_some : match_some ;
  }
and match_variant = {
    constructor : constructor' ;
    proj : expression_variable ;
    body : expression ;
  }
and match_variant_list = match_variant list
and matching_expr =
  | Match_list of match_list
  | Match_option of match_option
  | Match_variant of match_variant_list

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }

and ascription = {
    anno_expr: expression ;
    type_annotation: type_expression ;
  }

and env_def_declaration = {
  expr : expression ;
  free_variables : free_variables ;
}
and environment_element_definition =
  | ED_binder
  | ED_declaration of env_def_declaration

and free_variables = expression_variable list

and environment_element = {
    type_value: type_expression ;
    source_environment: environment ;
    definition: environment_element_definition ;
  }

and expr_env_binding = {
    binder : expression_variable ;
    element : environment_element ;
  }

and expr_environment = expr_env_binding list

and type_env_binding = {
    binder : type_variable ;
    element : type_expression ;
}
and type_environment = type_env_binding list

and environment = {
  expr_environment : expr_environment ;
  type_environment : type_environment ;
}

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
[@@@warning "-30"]

open Types_utils
include Stage_common.Enums (*@ follow ../common/enums.ml *)

(* pseudo-typeclasses: interfaces that must be provided for arguments
   of the givent polymmorphic types. For now, only one typeclass can
   be specified for a given polymorphic type. The implementation is
   provided by the Comparable module *)
(*@ typeclass poly_unionfind comparable *)
(*@ typeclass poly_set       comparable *)

type te_lmap = row_element label_map
and type_meta = ast_core_type_expression option

and type_content =
  | T_sum of te_lmap
  | T_record of te_lmap
  | T_arrow of arrow
  | T_variable of type_variable
  (* TODO: remove this when we remove the old typer *)
  | T_wildcard
  | T_constant of type_operator

and arrow = {
    type1: type_expression;
    type2: type_expression;
  }

and te_list = type_expression list
and type_operator = {
    type_constant : type_constant;
    arguments     : te_list;
  }

and annot_option = string option

and row_element = {
    associated_type : type_expression;
    michelson_annotation : annot_option;
    decl_pos : int;
}

and type_map_args = {
    k : type_expression;
    v : type_expression;
  }

and michelson_or_args = {
    l : type_expression;
    r : type_expression;
  }

and type_expression = {
    type_content: type_content;
    type_meta: type_meta;
    location: location;
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

and matching_content_case = {
    constructor : label ;
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
  | E_raw_code of raw_code
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

and raw_code = {
  language : string;
  code : expression;
}

and recursive = {
  fun_name : expression_variable;
  fun_type : type_expression;
  lambda : lambda;
}

and constructor = {
    constructor: label;
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

type row_tag =
  | C_record    (* ( label , * ) … -> * *)
  | C_variant   (* ( label , * ) … -> * *)

(* TODO: rename to type_expression or something similar (it includes variables, and unevaluated functions + applications *)
type type_value_ =
  | P_forall       of p_forall
  | P_variable     of type_variable
  | P_constant     of p_constant
  | P_apply        of p_apply
  | P_row          of p_row

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

and tv_lmap = type_value label_map
and p_row = {
    p_row_tag  : row_tag ;
    p_row_args : tv_lmap ;
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
and c_row_simpl_list         = c_row_simpl         list
and constraints = {
  (* If implemented in a language with decent sets, these should be sets not lists. *)
  constructor : c_constructor_simpl_list ; (* List of ('a = constructor(args…)) constraints *)
  poly        : c_poly_simpl_list        ; (* List of ('a = forall 'b, some_type) constraints *)
  tc          : c_typeclass_simpl_list   ; (* List of (typeclass(args…)) constraints *)
  row         : c_row_simpl_list         ; (* List of ('a = row (args..)) constraints *)
}
and type_variable_list = type_variable list
and type_variable_lmap = type_variable label_map
and c_constructor_simpl = {
  reason_constr_simpl : string ;
  tv : type_variable;
  c_tag : constant_tag;
  tv_list : type_variable_list;
}
and c_row_simpl = {
  reason_row_simpl : string ;
  tv : type_variable;
  r_tag : row_tag;
  tv_map : type_variable_lmap;
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
  | SC_Row         of c_row_simpl                     (* α = row(l -> β, …) *)

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

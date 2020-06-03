type    unionfind             =    Ast_typed.unionfind
type    constant_tag          =    Ast_typed.constant_tag
type    accessor              =    Ast_typed.label
type    type_value            =    Ast_typed.type_value
type    p_constraints         =    Ast_typed.p_constraints
type    p_forall              =    Ast_typed.p_forall
type    simple_c_constructor  =    Ast_typed.simple_c_constructor
type    simple_c_constant     =    Ast_typed.simple_c_constant
type    c_const               =    Ast_typed.c_const
type    c_equation            =    Ast_typed.c_equation
type    c_typeclass           =    Ast_typed.c_typeclass
type    c_access_label        =    Ast_typed.c_access_label
type    type_constraint       =    Ast_typed.type_constraint
type    typeclass             =    Ast_typed.typeclass
type 'a typeVariableMap       = 'a Ast_typed.typeVariableMap
type    structured_dbs        =    Ast_typed.structured_dbs
type    constraints           =    Ast_typed.constraints
type    c_constructor_simpl   =    Ast_typed.c_constructor_simpl
type    c_const_e             =    Ast_typed.c_const_e
type    c_equation_e          =    Ast_typed.c_equation_e
type    c_typeclass_simpl     =    Ast_typed.c_typeclass_simpl
type    c_poly_simpl          =    Ast_typed.c_poly_simpl
type    type_constraint_simpl =    Ast_typed.type_constraint_simpl
type    state                 =    Solver_types.typer_state

type type_variable = Ast_typed.type_variable
type type_expression = Ast_typed.type_expression

(* generate a new type variable and gave it an id *)
let fresh_type_variable : ?name:string -> unit -> type_variable =
    Var.fresh

open Trace
let type_expression'_of_simple_c_constant : constant_tag * type_expression list -> Ast_typed.type_content result = fun (c, l) ->
  match c, l with
  | C_contract  , [x]     -> ok @@ Ast_typed.T_operator(TC_contract x)
  | C_option    , [x]     -> ok @@ Ast_typed.T_operator(TC_option x)
  | C_list      , [x]     -> ok @@ Ast_typed.T_operator(TC_list x)
  | C_set       , [x]     -> ok @@ Ast_typed.T_operator(TC_set x)
  | C_map       , [k ; v] -> ok @@ Ast_typed.T_operator(TC_map {k ; v})
  | C_big_map   , [k ; v] -> ok @@ Ast_typed.T_operator(TC_big_map {k ; v})
  | C_arrow     , [x ; y] -> ok @@ Ast_typed.T_arrow {type1=x ; type2=y} (* For now, the arrow type constructor is special *)
  | C_record    , _lst    -> ok @@ failwith "records are not supported yet: T_record lst"
  | C_variant   , _lst    -> ok @@ failwith "sums are not supported yet: T_sum lst"
  | (C_contract | C_option | C_list | C_set | C_map | C_big_map | C_arrow ), _ ->
     failwith "internal error: wrong number of arguments for type operator"

  | C_unit      , [] -> ok @@ Ast_typed.T_constant(TC_unit)
  | C_string    , [] -> ok @@ Ast_typed.T_constant(TC_string)
  | C_bytes     , [] -> ok @@ Ast_typed.T_constant(TC_bytes)
  | C_nat       , [] -> ok @@ Ast_typed.T_constant(TC_nat)
  | C_int       , [] -> ok @@ Ast_typed.T_constant(TC_int)
  | C_mutez     , [] -> ok @@ Ast_typed.T_constant(TC_mutez)
  | C_operation , [] -> ok @@ Ast_typed.T_constant(TC_operation)
  | C_address   , [] -> ok @@ Ast_typed.T_constant(TC_address)
  | C_key       , [] -> ok @@ Ast_typed.T_constant(TC_key)
  | C_key_hash  , [] -> ok @@ Ast_typed.T_constant(TC_key_hash)
  | C_chain_id  , [] -> ok @@ Ast_typed.T_constant(TC_chain_id)
  | C_signature , [] -> ok @@ Ast_typed.T_constant(TC_signature)
  | C_timestamp , [] -> ok @@ Ast_typed.T_constant(TC_timestamp)
  | (C_unit | C_string | C_bytes | C_nat | C_int | C_mutez | C_operation | C_address | C_key | C_key_hash | C_chain_id | C_signature | C_timestamp), _::_ ->
     failwith "internal error: wrong number of arguments for type constant"

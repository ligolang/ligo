(* TODO: remove these and use the proper qualification where needed *)
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
type    c_constructor_simpl   =    Ast_typed.c_constructor_simpl
type    c_const_e             =    Ast_typed.c_const_e
type    c_equation_e          =    Ast_typed.c_equation_e
type    c_typeclass_simpl     =    Ast_typed.c_typeclass_simpl
type    c_poly_simpl          =    Ast_typed.c_poly_simpl
type    type_constraint_simpl =    Ast_typed.type_constraint_simpl
(* type    'errors state         =    'errors Solver_types.typer_state *)

type type_variable = Ast_typed.type_variable
type type_expression = Ast_typed.type_expression

(* generate a new type variable and gave it an id *)
let fresh_type_variable : ?name:string -> unit -> type_variable = fun ?name () ->
  let fresh_name = Var.fresh ?name () in
  let () = (if Ast_typed.Debug.debug_new_typer && false then Printf.fprintf stderr "Generated variable %s\n%!%s\n%!" (Var.debug fresh_name) (Printexc.get_backtrace ())) in
  fresh_name

let type_expression'_of_simple_c_constant : constant_tag * type_expression list -> Ast_typed.type_content option = fun (c, l) ->
  let return (x:type_expression) = Some x.type_content in
  match c, l with
  | C_contract  , [x]     -> return (Ast_typed.t_contract x)
  | C_option    , [x]     -> return (Ast_typed.t_option x)
  | C_list      , [x]     -> return (Ast_typed.t_list x)
  | C_set       , [x]     -> return (Ast_typed.t_set x)
  | C_map       , [k ; v] -> return (Ast_typed.t_map k v)
  | C_big_map   , [k ; v] -> return (Ast_typed.t_big_map k v)
  | C_arrow     , [x ; y] -> Some (Ast_typed.T_arrow {type1=x ; type2=y}) (* For now, the arrow type constructor is special *)
  | (C_contract | C_option | C_list | C_set | C_map | C_big_map | C_arrow ), _ -> None

  | C_unit      , [] -> return (Ast_typed.t_unit ())
  | C_string    , [] -> return (Ast_typed.t_string ())
  | C_bytes     , [] -> return (Ast_typed.t_bytes ())
  | C_nat       , [] -> return (Ast_typed.t_nat ())
  | C_int       , [] -> return (Ast_typed.t_int ())
  | C_mutez     , [] -> return (Ast_typed.t_mutez ())
  | C_operation , [] -> return (Ast_typed.t_operation ())
  | C_address   , [] -> return (Ast_typed.t_address ())
  | C_key       , [] -> return (Ast_typed.t_key ())
  | C_key_hash  , [] -> return (Ast_typed.t_key_hash ())
  | C_chain_id  , [] -> return (Ast_typed.t_chain_id ())
  | C_signature , [] -> return (Ast_typed.t_signature ())
  | C_timestamp , [] -> return (Ast_typed.t_timestamp ())
  | (C_unit | C_string | C_bytes | C_nat | C_int | C_mutez | C_operation | C_address | C_key | C_key_hash | C_chain_id | C_signature | C_timestamp), _::_ ->
      None

let type_expression'_of_simple_c_row : Ast_typed.row_tag * type_expression Ast_typed.label_map -> Ast_typed.type_content =
  fun (tag, map) ->
  let open Ast_typed in
  (*TODO : layout/annotations/decl_pos should be extracted from earlier passes *)
  let aux : type_expression -> row_element = fun te ->
    { associated_type = te ; michelson_annotation = None ; decl_pos = 0 }
  in
  let content = LMap.map aux map in
  match tag with
  | C_record -> T_record { layout = default_layout ; content }
  | C_variant -> T_sum { layout = default_layout ; content }

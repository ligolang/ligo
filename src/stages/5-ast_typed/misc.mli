open Types

val assert_value_eq : ( expression * expression ) -> unit option
val assert_type_expression_eq : ( type_expression * type_expression ) -> unit option
val merge_annotation :
  type_expression option ->
  type_expression option ->
  (type_expression * type_expression -> 'a option) -> type_expression option
val type_expression_eq : ( type_expression * type_expression ) -> bool

val equal_variables : expression -> expression -> bool

module Free_variables : sig
  type bindings = expression_variable list

  val matching_expression : bindings -> matching_expr -> bindings
  val lambda : bindings -> lambda -> bindings

  val expression : bindings -> expression -> bindings 

  val empty : bindings 
  val singleton : expression_variable -> bindings 
end

(*
val assert_literal_eq : ( literal * literal ) -> unit result
*)

val get_entry : program_fully_typed -> string -> expression option

val p_constant : constant_tag -> p_ctor_args -> type_value
val p_row      : row_tag      -> tv_lmap     -> type_value
val p_row_ez   : row_tag      -> (string * type_value) list -> type_value
val c_equation : type_value -> type_value -> string -> type_constraint

val reason_simpl : type_constraint_simpl -> string

val layout_eq : layout -> layout -> bool

val assert_eq : 'a -> 'a -> unit option
val assert_list_eq : ('a -> 'a -> unit option) -> 'a list -> 'a list -> unit option

val is_mandatory_constraint : type_constraint_simpl -> bool

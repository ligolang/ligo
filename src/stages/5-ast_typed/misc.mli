open Types

val assert_type_expression_eq : ( type_expression * type_expression ) -> unit option
val merge_annotation :
  type_expression option ->
  type_expression option ->
  (type_expression * type_expression -> 'a option) -> type_expression option
val type_expression_eq : ( type_expression * type_expression ) -> bool


module Free_variables : sig
  type bindings = expression_variable list

  val lambda : bindings -> lambda -> bindings

end

val get_entry : program -> expression_variable -> expression option

val layout_eq : layout -> layout -> bool

val assert_eq : 'a -> 'a -> unit option
val assert_list_eq : ('a -> 'a -> unit option) -> 'a list -> 'a list -> unit option
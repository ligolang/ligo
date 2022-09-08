open Types

val assert_type_expression_eq : ( type_expression * type_expression ) -> unit option
val type_expression_eq : ( type_expression * type_expression ) -> bool


module Free_variables : sig
  type bindings = Ligo_prim.Value_var.t list

  val lambda : bindings -> (expr,type_expression) Ligo_prim.Lambda.t -> bindings

end

val get_entry : program -> Ligo_prim.Value_var.t -> expression option


val assert_eq : 'a -> 'a -> unit option
val assert_list_eq : ('a -> 'a -> unit option) -> 'a list -> 'a list -> unit option

val get_type_of_contract : type_content -> (type_expression * type_expression) option

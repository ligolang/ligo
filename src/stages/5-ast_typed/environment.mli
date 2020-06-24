open Types

type t = environment
type element = environment_element

val empty : t
val add_ez_binder : expression_variable -> type_expression -> t -> t
val add_ez_declaration : expression_variable -> expression -> t -> t
val add_type : type_variable -> type_expression -> t -> t
val get_opt : expression_variable -> t -> element option
val get_type_opt : type_variable -> t -> type_expression option
val get_constructor : Ast_core.constructor' -> t -> (type_expression * type_expression) option
val convert_constructor' : S.constructor' -> constructor'

val add_ez_sum_type : ?env:environment -> ?type_name:type_variable -> (constructor' * ctor_content) list ->  environment
module PP : sig
  open Format

  val list_sep_scope : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  val environment : formatter -> environment -> unit 

end

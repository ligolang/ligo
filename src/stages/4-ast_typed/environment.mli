open Types
open Trace

type t = full_environment
type element = environment_element

val get_trace : expression_variable -> t -> element result
val empty : environment
val full_empty : t
val add : expression_variable -> element -> t -> t
val add_ez_binder : expression_variable -> type_expression -> t -> t
val add_ez_declaration : expression_variable -> expression -> t -> t
val add_ez_ae : expression_variable -> expression -> t -> t
val add_type : type_variable -> type_expression -> t -> t
val get_opt : expression_variable -> t -> element option
val get_type_opt : type_variable -> t -> type_expression option
val get_constructor : constructor' -> t -> (type_expression * type_expression) option

module Small : sig
  type t = small_environment
  val get_environment : t -> environment
(*

  val empty : t

  val get_type_environment : t -> type_environment
  val map_environment : ( environment -> environment ) -> t -> t
  val map_type_environment : ( type_environment -> type_environment ) -> t -> t

  val add : string -> element -> t -> t
  val add_type : string -> type_expression -> t -> t
  val get_opt : string -> t -> element option
  val get_type_opt : string -> t -> type_expression option
  *)
end
(*

val make_element : type_expression -> full_environment -> environment_element_definition -> element
val make_element_binder : type_expression -> full_environment -> element
val make_element_declaration : full_environment -> expression -> element
*)



module PP : sig
  open Format

  val list_sep_scope : (formatter -> 'a -> unit) -> formatter -> 'a list -> unit
  val full_environment : formatter -> full_environment -> unit 
(*
  val environment_element : formatter -> ( string * environment_element ) -> unit

  val type_environment_element : formatter -> ( string * type_expression ) -> unit

  val environment : formatter -> environment -> unit

  val type_environment : formatter -> type_environment -> unit

  val small_environment : formatter -> small_environment -> unit

  *)
end

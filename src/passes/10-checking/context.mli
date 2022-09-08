(* This file represente the context which give the association of values to types *)
open Ligo_prim
open Ast_typed
open Simple_utils.Trace

module Exists_var : sig
  type t = private type_variable [@@deriving compare]

  val pp : Format.formatter -> t -> unit
  val yojson_of_t : t -> Yojson.Safe.t
  val loc : t -> Location.t
  val equal : t -> t -> bool
  val of_type_var : type_variable -> t option
  val fresh : ?loc:Location.t -> unit -> t
  val of_type_var_exn : type_variable -> t
end

module Signature : sig
  type t = item list

  and item =
    | S_value of expression_variable * type_expression
    | S_type of type_variable * type_expression
    | S_module of module_variable * t

  val get_value : t -> expression_variable -> type_expression option
  val get_type : t -> type_variable -> type_expression option
  val get_module : t -> module_variable -> t option
  val pp : Format.formatter -> t -> unit
  val pp_item : Format.formatter -> item -> unit
end

type exists_variable = Exists_var.t

type t
and pos

and item =
  | C_value of expression_variable * type_expression
  | C_type of type_variable * type_expression
  | C_type_var of type_variable * Kind.t
  | C_exists_var of exists_variable * Kind.t
  | C_exists_eq of exists_variable * Kind.t * type_expression
  | C_marker of exists_variable
  | C_module of module_variable * Signature.t
  | C_pos of pos

val empty : t
val add : t -> item -> t
val of_list : item list -> t
val ( |:: ) : t -> item -> t
val join : t -> t -> t
val ( |@ ) : t -> t -> t
val pp : Format.formatter -> t -> unit
val pp_ : pos:pos -> Format.formatter -> t -> unit
val add_value : t -> expression_variable -> type_expression -> t
val add_type : t -> type_variable -> type_expression -> t
val add_type_var : t -> type_variable -> Kind.t -> t
val add_exists_var : t -> exists_variable -> Kind.t -> t
val add_marker : t -> exists_variable -> t
val add_module : t -> module_variable -> Signature.t -> t
val get_value : t -> expression_variable -> type_expression option
val get_type : t -> type_variable -> type_expression option
val get_module : t -> module_variable -> Signature.t option
val get_type_vars : t -> type_variable list
val get_exists_vars : t -> exists_variable list
val get_type_var : t -> type_variable -> Kind.t option
val get_exists_var : t -> exists_variable -> Kind.t option
val add_exists_eq : t -> exists_variable -> Kind.t -> type_expression -> t
val get_exists_eq : t -> exists_variable -> type_expression option
val get_signature : t -> module_variable List.Ne.t -> Signature.t option
val add_signature_item : t -> Signature.item -> t
val remove_pos : t -> pos:pos -> t
val insert_at : t -> at:item -> hole:t -> t
val split_at : t -> at:item -> t * t
val drop_until : t -> pos:pos -> t
val apply : t -> type_expression -> type_expression
val mark : t -> t * pos

val get_record
  :  type_expression Rows.row_element_mini_c Rows.LMap.t
  -> t
  -> (type_variable option * rows) option

val get_sum
  :  Label.t
  -> t
  -> (type_variable * type_variable list * type_expression * type_expression) list

module Well_formed : sig
  val context : t -> bool
  val type_expr : ctx:t -> type_expression -> Kind.t option
end

module Elaboration : sig
  type context := t
  type ('a, 'err, 'wrn) t

  include Monad.S3 with type ('a, 'err, 'wrn) t := ('a, 'err, 'wrn) t

  type error = [ `Typer_existential_found of Location.t * type_expression ]
  val raise : (('err, 'wrn) raise, 'err, 'wrn) t

  val all_lmap : ('a, 'err, 'wrn) t Rows.LMap.t -> ('a Rows.LMap.t, 'err, 'wrn) t
  val run_expr : (expression, [> error] as 'err, 'wrn) t -> ctx:context -> raise:('err, 'wrn) raise -> expression
  val run_decl : (decl, [> error] as 'err, 'wrn) t -> ctx:context -> raise:('err, 'wrn) raise -> decl
  val run_declaration : (declaration, [> error] as 'err, 'wrn) t -> ctx:context -> raise:('err, 'wrn) raise -> declaration

  val run_module : (module_, [> error] as 'err, 'wrn)  t -> ctx:context -> raise:('err, 'wrn) raise -> module_
  val run_program : (program, [> error] as 'err, 'wrn)  t -> ctx:context -> raise:('err, 'wrn) raise -> program
end

val enter
  :  ctx:t
  -> in_:(t -> t * type_expression * (expression, 'err, 'wrn) Elaboration.t)
  -> t * type_expression * (expression, 'err, 'wrn) Elaboration.t

val decl_enter : ctx:t -> in_:(t -> t * Signature.t * 'a) -> t * Signature.t * 'a

module Generalization : sig
  val enter
    :  ctx:t
    -> in_:(t -> t * type_expression * (expression, 'err, 'wrn) Elaboration.t)
    -> t * type_expression * (expression, 'err, 'wrn) Elaboration.t
end

val init : ?env:Environment.t -> unit -> t

module Hashes : sig
  val set_context : t -> unit
  val hash_types : unit -> unit
  val find_type : type_expression -> (module_variable list * type_variable) option
end

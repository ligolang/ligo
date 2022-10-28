open Simple_utils.Trace
open Ligo_prim
module List = Simple_utils.List
module Location = Simple_utils.Location

type ('a, 'err, 'wrn) t

include Monad.S3 with type ('a, 'err, 'wrn) t := ('a, 'err, 'wrn) t

val all_lmap
  :  ('a, 'err, 'wrn) t Record.LMap.t
  -> ('a Record.LMap.t, 'err, 'wrn) t

val all_lmap_unit : (unit, 'err, 'wrn) t Record.LMap.t -> (unit, 'err, 'wrn) t
val loc : unit -> (Location.t, 'err, 'wrn) t
val set_loc : Location.t -> ('a, 'err, 'wrn) t -> ('a, 'err, 'wrn) t
val hash_context : unit -> (unit, 'err, 'wrn) t
val context : unit -> (Context.t, 'err, 'wrn) t
val lift_raise : (('err, 'wrn) raise -> 'a) -> ('a, 'err, 'wrn) t

val raise_result
  :  ('a, 'b) result
  -> error:('b -> 'err Errors.with_loc)
  -> ('a, 'err, 'wrn) t

val raise_opt : 'a option -> error:'err Errors.with_loc -> ('a, 'err, 'wrn) t
val raise : 'err Errors.with_loc -> ('a, 'err, 'wrn) t
val raise_l : loc:Location.t -> 'err Errors.with_loc -> ('a, 'err, 'wrn) t
val warn : 'wrn Errors.with_loc -> (unit, 'err, 'wrn) t

type (_, _) exit =
  | Drop : ('a, 'a) exit
  | Lift_type : (Type.t * 'a, Type.t * 'a) exit
  | Lift_sig : (Context.Signature.t * 'a, Context.Signature.t * 'a) exit

module Context : sig
  module Signature = Context.Signature

  val get_value
    :  Value_var.t
    -> error:([ `Mut_var_captured | `Not_found ] -> 'err Errors.with_loc)
    -> (Context.mutable_flag * Type.t, 'err, 'wrn) t

  val get_imm
    :  Value_var.t
    -> error:'err Errors.with_loc
    -> (Type.t, 'err, 'wrn) t

  val get_mut
    :  Value_var.t
    -> error:'err Errors.with_loc
    -> (Type.t, 'err, 'wrn) t

  val get_type_var
    :  Type_var.t
    -> error:'err Errors.with_loc
    -> (Kind.t, 'err, 'wrn) t

  val get_type
    :  Type_var.t
    -> error:'err Errors.with_loc
    -> (Type.t, 'err, 'wrn) t

  val get_module
    :  Module_var.t
    -> error:'err Errors.with_loc
    -> (Signature.t, 'err, 'wrn) t

  val get_signature
    :  Module_var.t List.Ne.t
    -> error:'err Errors.with_loc
    -> (Signature.t, 'err, 'wrn) t

  val get_sum
    :  Label.t
    -> ((Type_var.t * Type_var.t list * Type.t * Type.t) list, 'err, 'wrn) t

  val get_record
    :  Type.row_element Record.t
    -> ((Type_var.t option * Type.row) option, 'err, 'wrn) t

  val lock
    :  on_exit:('a, 'b) exit
    -> in_:('a, 'err, 'wrn) t
    -> ('b, 'err, 'wrn) t

  val tapply : Type.t -> (Type.t, 'err, 'wrn) t

  module Well_formed : sig
    val context : unit -> (bool, 'err, 'wrn) t
    val type_ : Type.t -> (Kind.t option, 'err, 'wrn) t
  end
end

type unify_error =
  [ `Typer_cannot_unify of Type.t * Type.t * Location.t
  | `Typer_cannot_unify_diff_layout of
    Type.t * Type.t * Type.layout * Type.layout * Location.t
  | `Typer_ill_formed_type of Type.t * Location.t
  | `Typer_occurs_check_failed of Type_var.t * Type.t * Location.t
  | `Typer_unbound_texists_var of Type_var.t * Location.t
  ]

val unify : Type.t -> Type.t -> (unit, [> unify_error ], 'wrn) t

type subtype_error = unify_error

val subtype
  :  received:Type.t
  -> expected:Type.t
  -> ( Ast_typed.expression -> Ast_typed.expression Elaboration.t
     , [> subtype_error ]
     , 'wrn )
     t

val exists : Kind.t -> (Type.t, 'err, 'wrn) t
val for_all : Kind.t -> (Type.t, 'err, 'wrn) t

val create_type
  :  ?meta:Ast_core.type_expression
  -> Type.constr
  -> (Type.t, 'err, 'wrn) t

val def
  :  (Value_var.t * Param.mutable_flag * Type.t) list
  -> on_exit:('a, 'b) exit
  -> in_:('a, 'err, 'wrn) t
  -> ('b, 'err, 'wrn) t

val def_type
  :  (Type_var.t * Type.t) list
  -> on_exit:('a, 'b) exit
  -> in_:('a, 'err, 'wrn) t
  -> ('b, 'err, 'wrn) t

val def_type_var
  :  (Type_var.t * Kind.t) list
  -> on_exit:('a, 'b) exit
  -> in_:('a, 'err, 'wrn) t
  -> ('b, 'err, 'wrn) t

val def_module
  :  (Module_var.t * Context.Signature.t) list
  -> on_exit:('a, 'b) exit
  -> in_:('a, 'err, 'wrn) t
  -> ('b, 'err, 'wrn) t

val def_sig_item
  :  Context.Signature.item list
  -> on_exit:('a, 'b) exit
  -> in_:('a, 'err, 'wrn) t
  -> ('b, 'err, 'wrn) t

val generalize
  :  (Type.t * 'a, 'err, 'wrn) t
  -> (Type.t * Type_var.t list * 'a, 'err, 'wrn) t

val assert_ : bool -> error:'err Errors.with_loc -> (unit, 'err, 'wrn) t
val fresh_type_var : unit -> (Type_var.t, 'err, 'wrn) t

val try_
  :  ('a, 'err, 'wrn) t
  -> with_:('err -> ('a, 'err, 'wrn) t)
  -> ('a, 'err, 'wrn) t

module With_frag : sig
  type ('a, 'err, 'wrn) e := ('a, 'err, 'wrn) t
  type ('a, 'err, 'wrn) t
  type fragment = (Value_var.t * Param.mutable_flag * Type.t) list

  include Monad.S3 with type ('a, 'err, 'wrn) t := ('a, 'err, 'wrn) t

  val lift : ('a, 'err, 'wrn) e -> ('a, 'err, 'wrn) t

  val create_type
    :  ?meta:Ast_core.type_expression
    -> Type.constr
    -> (Type.t, 'err, 'wrn) t

  val all_lmap
    :  ('a, 'err, 'wrn) t Record.LMap.t
    -> ('a Record.LMap.t, 'err, 'wrn) t

  val all_lmap_unit : (unit, 'err, 'wrn) t Record.LMap.t -> (unit, 'err, 'wrn) t
  val loc : unit -> (Location.t, 'err, 'wrn) t
  val set_loc : Location.t -> ('a, 'err, 'wrn) t -> ('a, 'err, 'wrn) t

  val raise_result
    :  ('a, 'b) result
    -> error:('b -> 'err Errors.with_loc)
    -> ('a, 'err, 'wrn) t

  val raise_opt : 'a option -> error:'err Errors.with_loc -> ('a, 'err, 'wrn) t
  val raise : 'err Errors.with_loc -> ('a, 'err, 'wrn) t
  val raise_l : loc:Location.t -> 'err Errors.with_loc -> ('a, 'err, 'wrn) t
  val warn : 'wrn Errors.with_loc -> (unit, 'err, 'wrn) t
  val assert_ : bool -> error:'err Errors.with_loc -> (unit, 'err, 'wrn) t

  module Context : sig
    val get_sum
      :  Label.t
      -> ((Type_var.t * Type_var.t list * Type.t * Type.t) list, 'err, 'wrn) t

    val get_record
      :  Type.row_element Record.t
      -> ((Type_var.t option * Type.row) option, 'err, 'wrn) t

    val tapply : Type.t -> (Type.t, 'err, 'wrn) t
  end

  val exists : Kind.t -> (Type.t, 'err, 'wrn) t
  val unify : Type.t -> Type.t -> (unit, [> unify_error ], 'wrn) t

  val subtype
    :  received:Type.t
    -> expected:Type.t
    -> ( Ast_typed.expression -> Ast_typed.expression Elaboration.t
       , [> subtype_error ]
       , 'wrn )
       t

  val extend : fragment -> (unit, 'err, 'wrn) t
  val run : ('a, 'err, 'wrn) t -> (fragment * 'a, 'err, 'wrn) e
end

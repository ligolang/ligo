open Simple_utils.Trace
open Ligo_prim
module List = Simple_utils.List
module Location = Simple_utils.Location

(** [('a, 'err, 'wrn) t] computation returns a value of type ['a], potentially 
    raising errors and warnings of type ['err] and ['wrn]. *)
type ('a, 'err, 'wrn) t

include Monad.S3 with type ('a, 'err, 'wrn) t := ('a, 'err, 'wrn) t

val all_lmap
  :  ('a, 'err, 'wrn) t Record.LMap.t
  -> ('a Record.LMap.t, 'err, 'wrn) t

val all_lmap_unit : (unit, 'err, 'wrn) t Record.LMap.t -> (unit, 'err, 'wrn) t

(** {1 Location Handling} *)

(** [loc ()] returns the current location *)
val loc : unit -> (Location.t, 'err, 'wrn) t

(** [set_loc loc t] sets the current location to [loc] in computation [t] *)
val set_loc : Location.t -> ('a, 'err, 'wrn) t -> ('a, 'err, 'wrn) t

(** {2 Error Handling} *)

(** [lift_raise f] lifts f (which may raise an error/warning) to a computation. *)
val lift_raise : (('err, 'wrn) raise -> 'a) -> ('a, 'err, 'wrn) t

(** [raise err] raises the error [err] at location [loc ()] (the current location). *)
val raise : 'err Errors.with_loc -> ('a, 'err, 'wrn) t

(** [raise_l ~loc err] raises the error [err] at location [loc]. *)
val raise_l : loc:Location.t -> 'err Errors.with_loc -> ('a, 'err, 'wrn) t

(** [warn wrn] reports the warning [wrn] at location [loc ()] (the current location). *)
val warn : 'wrn Errors.with_loc -> (unit, 'err, 'wrn) t

(** [raise_result ~error result] simply lifts the value of the result into a computation. 
    If [result] is [Error err], then the computation is equivalent to [raise (error err)]. *)
val raise_result
  :  ('a, 'b) result
  -> error:('b -> 'err Errors.with_loc)
  -> ('a, 'err, 'wrn) t

(** [raise_opt ~error opt] lifts the value of the option into a computation.
    If [opt] is [None], then the computation raises [error]. *)
val raise_opt : 'a option -> error:'err Errors.with_loc -> ('a, 'err, 'wrn) t

(** [assert_ ~error cond] asserts that [cond] is [true]. If [false], then [error] 
    is raised. *)
val assert_ : bool -> error:'err Errors.with_loc -> (unit, 'err, 'wrn) t

(** [try_ comp ~with_] executes the computation [comp]. If [comp] raises 
    an error [err], then the handler [with_] is called with [err]. *)
val try_
  :  ('a, 'err, 'wrn) t
  -> with_:('err -> ('a, 'err, 'wrn) t)
  -> ('a, 'err, 'wrn) t

val try_all
  :  ('a, ([> `Typer_corner_case of string * Location.t ] as 'err), 'wrn) t list
  -> ('a, 'err, 'wrn) t

(** {3 Compiler Options} *)

(** [options ()] returns the compiler computations. *)
val options : unit -> (Compiler_options.middle_end, 'err, 'wrn) t

module Options : sig
  (** [test ()] returns whether the [--test] flag was passed via compiler options.  
      This is equivalent to [options () >>| fun opt -> opt.test] *)
  val test : unit -> (bool, 'err, 'wrn) t

  (** [syntax ()] returns the syntax passed by the [--syntax] flag. 
      This is equivalent to [options () >>| fun opt -> opt.syntax] *)
  val syntax : unit -> (Syntax_types.t option, 'err, 'wrn) t

  (** [no_color ()] returns whether the [--no-colour] flag was passed via compiler options.
      This is equivalent to [options () >>| fun opt -> opt.no_colour] *)
  val no_color : unit -> (bool, 'err, 'wrn) t
end

(** {4 Context} *)

(** [context ()] returns the context. *)
val context : unit -> (Context.t, 'err, 'wrn) t

(** [hash_context ()] hashes the current context (required for the hashing used to 
    improve types when reporting errors). *)
val hash_context : unit -> (unit, 'err, 'wrn) t

(** ['a exit] defines the behavior when exiting a context scope, returning
    a computation of type ['a] *)
type 'a exit =
  | Drop : 'a exit
      (** [Drop] simply drops all context items up until the scope marker *)
  | Lift_type : (Type.t * 'a) exit
      (** [Lift_type] drops everything up until the scope marker *except* existential variables 
          and applies any dropped equations to the returned type.
          
          Hint: You probably want to use this when exiting the scope during inference. *)
  | Lift_sig : (Context.Signature.t * 'a) exit
      (** Similar to [Lift_type] but for signatures. *)

module Context : sig
  module Signature = Context.Signature

  (** {1 Context lookup functions} *)

  (** [get_value var] returns the mutable flag and type of variable [var] in 
      the current context. 
      
      If unbound or if [var] is a mutable variable (and would be 
      captured by a closure), then we return [Error]. *)
  val get_value
    :  Value_var.t
    -> ( ( Context.mutable_flag * Type.t
         , [ `Mut_var_captured | `Not_found ] )
         result
       , 'err
       , 'wrn )
       t

  val get_value_exn
    :  Value_var.t
    -> error:([ `Mut_var_captured | `Not_found ] -> 'err Errors.with_loc)
    -> (Context.mutable_flag * Type.t, 'err, 'wrn) t

  (** [get_imm var] returns the type of the immutable variable [var].
      Returning [None] if not found in the current context. *)
  val get_imm : Value_var.t -> (Type.t option, 'err, 'wrn) t

  val get_imm_exn
    :  Value_var.t
    -> error:'err Errors.with_loc
    -> (Type.t, 'err, 'wrn) t

  (** [get_mut var] returns the type of the mutable variable [var].
      Returning [None] if not found in the current context. *)
  val get_mut : Value_var.t -> (Type.t option, 'err, 'wrn) t

  val get_mut_exn
    :  Value_var.t
    -> error:'err Errors.with_loc
    -> (Type.t, 'err, 'wrn) t

  (** [get_type_var tvar] returns the kind of the type variable [tvar].
      Returning [None] if not found in the current context. *)

  val get_type_var : Type_var.t -> (Kind.t option, 'err, 'wrn) t

  val get_type_var_exn
    :  Type_var.t
    -> error:'err Errors.with_loc
    -> (Kind.t, 'err, 'wrn) t

  (** [get_type tvar] returns the type bound to the type variable [tvar].
      Returning [None] if not found in the current context. *)

  val get_type : Type_var.t -> (Type.t option, 'err, 'wrn) t

  val get_type_exn
    :  Type_var.t
    -> error:'err Errors.with_loc
    -> (Type.t, 'err, 'wrn) t

  (** [get_module mvar] returns signature of the module [mvar].
      Returning [None] if not found in the current context. *)

  val get_module : Module_var.t -> (Signature.t option, 'err, 'wrn) t

  val get_module_exn
    :  Module_var.t
    -> error:'err Errors.with_loc
    -> (Signature.t, 'err, 'wrn) t

  (** [get_signature path] returns the signature of the module path [path].
      Returning [None] if not found in the current context. *)

  val get_signature
    :  Module_var.t List.Ne.t
    -> (Signature.t option, 'err, 'wrn) t

  val get_signature_exn
    :  Module_var.t List.Ne.t
    -> error:'err Errors.with_loc
    -> (Signature.t, 'err, 'wrn) t

  (** [get_sum constr] returns a list of [(type_name, type_params, constr_type, sum_type)] for any sum type in the context
      containing [constr].
      
      Note (for some reason): We return all the matching types found in the modules (and submodules) in the current context. 
      For example:
      {|
        module Mod_a = struct
          type tx = A of int
        end

        type ty = A of int

        let a = A 42
      |}
      returns both the types [tx] and [ty].
  *)

  val get_sum
    :  Label.t
    -> ((Type_var.t * Type_var.t list * Type.t * Type.t) list, 'err, 'wrn) t

  (** [get_record record_type] returns the record type defined in the context that instantiates
      to [record_type].  *)
  val get_record
    :  Type.row_element Record.t
    -> ((Type_var.t option * Type.row) option, 'err, 'wrn) t

  (** [lock ~on_exit ~in_] adds a local fitch-style lock for mutable variables to 
      the context in [in_]. *)

  val lock : on_exit:'a exit -> in_:('a, 'err, 'wrn) t -> ('a, 'err, 'wrn) t

  (** [tapply type_] applies the context substitution to [type_].
      
      Hint: you often want to use this when passing a type from an inference function
      to a checking function. *)

  val tapply : Type.t -> (Type.t, 'err, 'wrn) t

  module Well_formed : sig
    val context : unit -> (bool, 'err, 'wrn) t
    val type_ : Type.t -> (Kind.t option, 'err, 'wrn) t
  end
end

val fresh_type_var : unit -> (Type_var.t, 'err, 'wrn) t

(** [exists kind] creates a new existential variable of kind [kind] *)
val exists : Kind.t -> (Type.t, 'err, 'wrn) t

(** [for_all kind] creates a new universal variable of kind [kind] *)
val for_all : Kind.t -> (Type.t, 'err, 'wrn) t

(** [lexists ()] create a new existential layout variable *)
val lexists : unit -> (Type.layout, 'err, 'wrn) t

(** [create_type constr] returns a created type using the [constr] function
    with the location automatically provided *)
val create_type
  :  ?meta:Ast_core.type_expression
  -> Type.constr
  -> (Type.t, 'err, 'wrn) t

(** [def bindings ~on_exit ~in_] binds the context bindings [bindings] in 
    computation [in_] *)

val def
  :  (Value_var.t * Param.mutable_flag * Type.t) list
  -> on_exit:'a exit
  -> in_:('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

(** [def_type types ~on_exit ~in_] binds the type definitions [types] in 
    computation [in_] *)
val def_type
  :  (Type_var.t * Type.t) list
  -> on_exit:'a exit
  -> in_:('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

(** [def_type_var tvars ~on_exit ~in_] binds the universal type variables 
    in computation [in_]. 

    Similar to [fun (type a) -> ...] in OCaml.    
*)
val def_type_var
  :  (Type_var.t * Kind.t) list
  -> on_exit:'a exit
  -> in_:('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

(** [def_module modules ~on_exit ~in_] binds the module bindings [modules] in 
    computation [in_]. *)
val def_module
  :  (Module_var.t * Context.Signature.t) list
  -> on_exit:'a exit
  -> in_:('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

val def_sig_item
  :  Context.Signature.item list
  -> on_exit:'a exit
  -> in_:('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

val generalize
  :  (Type.t * 'a, 'err, 'wrn) t
  -> (Type.t * (Type_var.t * Kind.t) list * 'a, 'err, 'wrn) t

(** {5 Unification and Subtyping} *)

type unify_error =
  [ `Typer_cannot_unify of bool * Type.t * Type.t * Location.t
  | `Typer_cannot_unify_diff_layout of
    Type.t * Type.t * Type.layout * Type.layout * Location.t
  | `Typer_ill_formed_type of Type.t * Location.t
  | `Typer_occurs_check_failed of Type_var.t * Type.t * Location.t
  | `Typer_unbound_texists_var of Type_var.t * Location.t
  ]

(** [unify_texists tvar type_] unifies the existential (unification) variable [tvar] with the type [type_]. *)
val unify_texists : Type_var.t -> Type.t -> (unit, [> unify_error ], 'wrn) t

(** [unify type1 type2] unifies the types [type1] and [type2]. 
    
    Hint: In general, use [unify] over [subtype]. *)
val unify : Type.t -> Type.t -> (unit, [> unify_error ], 'wrn) t

type subtype_error = unify_error

(** [subtype ~received ~expected] ensures [received] is the subtype of [expected] 
    and returns a coercion [f] that accepts an expression of type [received] and 
    elaborates to an expression of type [expected]. *)

val subtype
  :  received:Type.t
  -> expected:Type.t
  -> ( Ast_typed.expression -> Ast_typed.expression Elaboration.t
     , [> subtype_error ]
     , 'wrn )
     t

(** {6 Fragments} *)

(** A fragment is defined as a collection of variable bindings. They're used in the typing
    of patterns. 
    
    For example:
      {|
        type recd = { a : int; b : bool; c : char }
        type foo = Bar of int * string * recd

        match foo with
        | Bar (x, y, { a; b; c }) -> ...
      |}
    would generate the fragment: [ [ "x", int; "y", string; "a", int; "b", bool; "c", char ] ]. 
*)

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
  val lexists : unit -> (Type.layout, 'err, 'wrn) t
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

(** {7 Execution}*)

(** This section defines functions related to running computations. *)

(** [encode type_] encodes an [Ast_typed.type_expression] representation of a type into the typer's 
    internal representation [Type.t]. *)
val encode : Ast_typed.type_expression -> Type.t

(** [run_elab comp ~raise ~options ~env] runs and elaborates the computation [comp] with the handler 
    provided by [~raise], compiler options [~options] and initial environment [~env]. *)
val run_elab
  :  ('a Elaboration.t, Errors.typer_error, Main_warnings.all) t
  -> raise:(Errors.typer_error, Main_warnings.all) raise
  -> options:Compiler_options.middle_end
  -> ?env:Environment.t
  -> unit
  -> 'a

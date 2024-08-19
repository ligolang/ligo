open Ligo_prim
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location

(** [('a, 'err, 'wrn) t] computation returns a value of type ['a], potentially
    raising errors and warnings of type ['err] and ['wrn]. *)
type ('a, 'err, 'wrn) t

include Monad.S3 with type ('a, 'err, 'wrn) t := ('a, 'err, 'wrn) t

module Make_all : functor
  (T : sig
     type 'a t

     val fold_map : ('acc -> 'a1 -> 'acc * 'a2) -> 'acc -> 'a1 t -> 'acc * 'a2 t
   end)
  -> sig
  val all : ('a, 'err, 'wrn) t T.t -> ('a T.t, 'err, 'wrn) t
end

val all_lmap : ('a, 'err, 'wrn) t Label.Map.t -> ('a Label.Map.t, 'err, 'wrn) t
val all_lmap_unit : (unit, 'err, 'wrn) t Label.Map.t -> (unit, 'err, 'wrn) t

(** {1 Module Path Handling} *)

(** [path ()] returns the current module path. *)
val path : unit -> (Module_var.t list, 'err, 'wrn) t

(** [set_path path t] sets the current module path to [path] in computation [t]. *)
val set_path : Module_var.t list -> ('a, 'err, 'wrn) t -> ('a, 'err, 'wrn) t

(** {2 Location Handling} *)

(** [loc ()] returns the current location. *)
val loc : unit -> (Location.t, 'err, 'wrn) t

(** [set_loc loc t] sets the current location to [loc] in computation [t]. *)
val set_loc : Location.t -> ('a, 'err, 'wrn) t -> ('a, 'err, 'wrn) t

(** {3 Name table for polymorphic types} *)

(** [poly_name_tbl ()] returns a name table for the current declaration. *)
val poly_name_tbl : unit -> (Type.Type_var_name_tbl.t, 'err, 'wrn) t

(** [set_poly_name_tbl poly_name_tbl t] sets the current name table to [poly_name_tbl] in computation [t]. *)
val set_poly_name_tbl
  :  Type.Type_var_name_tbl.t
  -> ('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

(** {4 Table with references} *)

(** [refs_tbl ()] returns a table with references. *)
val refs_tbl : unit -> (Context.Refs_tbl.t, 'err, 'wrn) t

(** [set_refs_tbl refs_tbl t] sets the current table with references to [refs_tbl] in computation [t]. *)
val set_refs_tbl : Context.Refs_tbl.t -> ('a, 'err, 'wrn) t -> ('a, 'err, 'wrn) t

(** {5 Error Handling} *)

(** [lift_raise f] lifts f (which may raise an error/warning) to a computation. *)
val lift_raise : (('err, 'wrn) Trace.raise -> 'a) -> ('a, 'err, 'wrn) t

(** [raise err] raises the error [err] at location [loc ()] (the current location). *)
val raise : 'err Errors.with_loc -> ('a, 'err, 'wrn) t

(** [log_error err] logs the error [err] at location [loc ()] (the current location). *)
val log_error : 'err Errors.with_loc -> (unit, 'err, 'wrn) t

(** [raise_l ~loc err] raises the error [err] at location [loc]. *)
val raise_l : loc:Location.t -> 'err Errors.with_loc -> ('a, 'err, 'wrn) t

(** [log_error_l ~loc err] logs the error [err] at location [loc]. *)
val log_error_l : loc:Location.t -> 'err Errors.with_loc -> (unit, 'err, 'wrn) t

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
    errors [errs], then the handler [with_] is called with [errs]. *)
val try_
  :  ('a, 'err, 'wrn) t
  -> with_:('err list -> ('a, 'err, 'wrn) t)
  -> ('a, 'err, 'wrn) t

(** [try_with_diagnostics comp ~with_ ~diagnostics] is like [try_], but it also allows
    to inspect generated diagnostics (errors and warnings) generated while trying [comp],
    regardless of success of failure. Useful for error recovery, used by the language
    server. *)
val try_with_diagnostics
  :  ('a, 'err, 'wrn) t
  -> with_:('a, 'err, 'wrn) t
  -> diagnostics:('err list -> 'wrn list -> (unit, 'err, 'wrn) t)
  -> ('a, 'err, 'wrn) t

val try_all
  :  ('a, ([> `Typer_corner_case of string * Location.t ] as 'err), 'wrn) t Nonempty_list.t
  -> ('a, 'err, 'wrn) t

(** {6 Compiler Options} *)

(** [options ()] returns the compiler computations. *)
val options : unit -> (Compiler_options.middle_end, 'err, 'wrn) t

module Options : sig
  (** [test ()] returns whether the [--test] flag was passed via compiler options.
      This is equivalent to [options () >>| fun opt -> opt.test] *)
  val test : unit -> (bool, 'err, 'wrn) t

  (** [syntax ()] returns the syntax passed by the [--syntax] flag.
      This is equivalent to [options () >>| fun opt -> opt.syntax] *)
  val syntax : unit -> (Syntax_types.t option, 'err, 'wrn) t

  (** [no_color ()] returns whether the [--no-color] flag was passed via compiler options.
      This is equivalent to [options () >>| fun opt -> opt.no_colour] *)
  val no_color : unit -> (bool, 'err, 'wrn) t

  (** [array_as_list ()] returns whether the [--feature-infer-array-as-list]
        flag was passed via compiler options.
      This is equivalent to [options () >>| fun opt -> opt.array_as_list] *)
  val array_as_list : unit -> (bool, 'err, 'wrn) t
end

(** {7 Context} *)

module Context_ = Context

(** [context ()] returns the context. *)
val context : unit -> (Context.t, 'err, 'wrn) t

(** [hash_context ()] hashes the current context (required for the hashing used to
    improve types when reporting errors). *)
val hash_context : unit -> (unit, 'err, 'wrn) t

(** ['a exit] defines the behavior when exiting a context scope, returning
    a computation of type ['a] *)
type 'a exit =
  | Drop : 'a exit (** [Drop] simply drops all context items up until the scope marker *)
  | Lift_type : (Type.t * 'a) exit
      (** [Lift_type] drops everything up until the scope marker *except* existential variables
          and applies any dropped equations to the returned type.

          Hint: You probably want to use this when exiting the scope during inference. *)
  | Lift_sig : (Context.Signature.t * 'a) exit
      (** Similar to [Lift_type] but for signatures. *)

module Context : sig
  module Attr = Context.Attrs.Value
  module Signature = Context.Signature

  (** {1 Context lookup functions} *)

  (** [get_value var] returns the mutable flag and type of variable [var] in
      the current context.

      If unbound or if [var] is a mutable variable (and would be
      captured by a closure), then we return [Error]. *)
  val get_value
    :  Value_var.t
    -> ( ( Context.mutable_flag * Type.t * Context.Attrs.Value.t
         , [ `Mut_var_captured | `Not_found ] )
         result
       , 'err
       , 'wrn )
       t

  (** [get_imm var] returns the type of the immutable variable [var].
      Returning [None] if not found in the current context. *)
  val get_imm : Value_var.t -> ((Type.t * Context.Attrs.Value.t) option, 'err, 'wrn) t

  (** [get_mut var] returns the type of the mutable variable [var].
      Returning [None] if not found in the current context. *)
  val get_mut
    :  Value_var.t
    -> ((Type.t, [ `Mut_var_captured | `Not_found ]) result, 'err, 'wrn) t

  (** [get_type_var tvar] returns the kind of the type variable [tvar].
      Returning [None] if not found in the current context.

      Warning: you may encouter shadowing issues if you also want to access
      declared types. e.g. [type foo = int in ...] *)

  val get_type_var : Type_var.t -> (Kind.t option, 'err, 'wrn) t

  (** [get_type tvar] returns the type bound to the type variable [tvar].
      Returning [None] if not found in the current context.

      Warning: you may encouter shadowing issues if you also want to access
      bound type variables. e.g. [fun (type a) ... -> ...] *)

  val get_type : Type_var.t -> (Type.t option, 'err, 'wrn) t

  (** [get_type_or_type_var tvar] returns the type or kind of the type variable [tvar].
      Returning [None] if not found in the current context. *)
  val get_type_or_type_var
    :  Type_var.t
    -> ([ `Type of Type.t | `Type_var of Kind.t ] option, 'err, 'wrn) t

  (** [get_module mvar] returns signature of the module [mvar].
      Returning [None] if not found in the current context. *)

  val get_module : Module_var.t -> (Signature.t option, 'err, 'wrn) t

  (** [get_module_of_path path] returns the signature of the module path [path].
      Returning [None] if not found in the current context. *)

  val get_module_of_path
    :  Module_var.t Nonempty_list.t
    -> (Signature.t option, 'err, 'wrn) t

  (** [get_module_type_of_path path] returns the signature of the module path [path].
      Returning [None] if not found in the current context. *)

  val get_module_type_of_path
    :  Module_var.t Nonempty_list.t
    -> (Signature.t option, 'err, 'wrn) t

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
    :  Type.t Label.Map.t
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
val lexists : Label.Set.t -> (Type.layout, 'err, 'wrn) t

(** [create_type constr] returns a created type using the [constr] function
    with the location automatically provided *)
val create_type : Type.constr -> (Type.t, 'err, 'wrn) t

(** [def bindings ~on_exit ~in_] binds the context bindings [bindings] in
    computation [in_] *)

val def
  :  (Value_var.t * Param.mutable_flag * Type.t * Context.Attr.t) list
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

(** [def_module modules ~on_exit ~in_] binds the module bindings [modules] in
    computation [in_]. *)
val def_module_type
  :  (Module_var.t * Context.Signature.t) list
  -> on_exit:'a exit
  -> in_:('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

val def_sig_item
  :  Context.Signature.item Location.wrap list
  -> on_exit:'a exit
  -> in_:('a, 'err, 'wrn) t
  -> ('a, 'err, 'wrn) t

val generalize
  :  (Type.t * 'a, 'err, 'wrn) t
  -> (Type.t * (Type_var.t * Kind.t) list * 'a, 'err, 'wrn) t

(** {9 Unification and Subtyping} *)

(** [unify_texists tvar type_] unifies the existential (unification) variable [tvar] with the type [type_]. *)
val unify_texists : Type_var.t -> Type.t -> (unit, [> Errors.local_unify_error ], 'wrn) t

(** [unify type1 type2] unifies the types [type1] and [type2].

    Hint: In general, use [unify] over [subtype]. *)
val unify : Type.t -> Type.t -> (unit, [> Errors.unify_error ], 'wrn) t

(** [eq type1 type2] unifies the types [type1] and [type2].

    Hint: In general, use [unify] over [subtype]. *)
val eq : Type.t -> Type.t -> (bool, [> Errors.local_unify_error ], 'wrn) t

(** [subtype ~received ~expected] ensures [received] is the subtype of [expected]
    and returns a coercion [f] that accepts an expression of type [received] and
    elaborates to an expression of type [expected]. *)
val subtype
  :  received:Type.t
  -> expected:Type.t
  -> ( Ast_typed.expression -> Ast_typed.expression Elaboration.t
     , [> Errors.subtype_error ]
     , 'wrn )
     t

val subtype_opt
  :  received:Type.t
  -> expected:Type.t
  -> ( (Ast_typed.expression -> Ast_typed.expression Elaboration.t) option
     , [> Errors.subtype_error ]
     , 'a )
     t

(** [lub2_without_union type1 type2] computes tries to compute
     a non-trivial (meaning not just [type1 | type2]) least upper bound
     (with respect to the subtyping order) of [type1] and [type2].
     - If it succeds, it returns [Some (lub, type1_to_lub, type2_to_lub)] where 
       * [lub] is the least upper bound, and
       * [typek_to_lub] is a coercion from [typek] to [lub].
     - Otherwise, it returns [None]. *)
val lub2_without_union
  :  Type.t
  -> Type.t
  -> ( (Type.t
       * (Ast_typed.expression -> Ast_typed.expression Elaboration.t)
       * (Ast_typed.expression -> Ast_typed.expression Elaboration.t))
       option
     , [> `Typer_cannot_subtype of Errors.local_unify_error * Type.t * Type.t * Location.t
       ]
     , 'a )
     t

(** [lub_without_union] is the generalization of the binary [lub2_without_union] to an arbitrary arity *)
val lub_without_union
  :  Type.t list
  -> ( (Type.t
       * (Type.t * (Ast_typed.expression -> Ast_typed.expression Elaboration.t)) list)
       option
     , [> `Typer_cannot_subtype of Errors.local_unify_error * Type.t * Type.t * Location.t
       ]
     , 'a )
     t

(** [lub_union [type1; ...; typen]] returns the trivial least upper bound `[type1 | ... | typen]` *)
val lub_union
  :  Type.t list
  -> ( Type.t
       * (Type.t * (Ast_typed.expression -> Ast_typed.expression Elaboration.t)) list
     , [> `Typer_cannot_subtype of Errors.local_unify_error * Type.t * Type.t * Location.t
       ]
     , 'a )
     t

(** [lub [type1; ...; typen]] tries to return the non-trivial least upper bound returned by [lub_without_union [type1; ...; typen]]
    and if that fails, it returns the trivial least upper bound [lub_union [type1; ...; typen]] *)
val lub
  :  Type.t list
  -> ( Type.t
       * (Type.t * (Ast_typed.expression -> Ast_typed.expression Elaboration.t)) list
     , [> `Typer_cannot_subtype of Errors.local_unify_error * Type.t * Type.t * Location.t
       ]
     , 'a )
     t

(** {8 Fragments} *)

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
  val create_type : Type.constr -> (Type.t, 'err, 'wrn) t
  val all_lmap : ('a, 'err, 'wrn) t Label.Map.t -> ('a Label.Map.t, 'err, 'wrn) t
  val all_lmap_unit : (unit, 'err, 'wrn) t Label.Map.t -> (unit, 'err, 'wrn) t
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
  val map_error : f:('err2 -> 'err1) -> ('a, 'err2, 'wrn) t -> ('a, 'err1, 'wrn) t

  module Context : sig
    val get_sum
      :  Label.t
      -> ((Type_var.t * Type_var.t list * Type.t * Type.t) list, 'err, 'wrn) t

    val get_record
      :  Type.t Label.Map.t
      -> ((Type_var.t option * Type.row) option, 'err, 'wrn) t

    val tapply : Type.t -> (Type.t, 'err, 'wrn) t
  end

  val exists : Kind.t -> (Type.t, 'err, 'wrn) t
  val lexists : Label.Set.t -> (Type.layout, 'err, 'wrn) t
  val unify : Type.t -> Type.t -> (unit, Errors.unify_error, 'wrn) t

  val subtype
    :  received:Type.t
    -> expected:Type.t
    -> ( Ast_typed.expression -> Ast_typed.expression Elaboration.t
       , Errors.subtype_error
       , 'wrn )
       t

  val extend : fragment -> (unit, 'err, 'wrn) t
  val run : ('a, 'err, 'wrn) t -> (fragment * 'a, 'err, 'wrn) e
end

(** {10 Execution} *)

(** This section defines functions related to running computations. *)

(** [encode ~raise type_] encodes an [Ast_typed.type_expression] representation of a type
    into the typer's internal representation [Type.t]. *)
val encode
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> Ast_typed.type_expression
  -> Type.t

val encode_signature
  :  raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> Ast_typed.signature
  -> Context.Signature.t

(** [run_elab_with_refs comp ~raise ~options ~loc ~refs_tbl ~env] runs and elaborates the
    computation [comp] with the handler provided by [~raise], compiler options [~options],
    table with references [~refs_tbl], and initial environment [~env].
    In addition to the result of [comp], it will also fill [~refs_tbl] with references
    collected during the elaboration. *)
val run_elab_with_refs
  :  ('a Elaboration.t, Errors.typer_error, Main_warnings.all) t
  -> raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> loc:Location.t
  -> path:Module_var.t list
  -> refs_tbl:Context_.Refs_tbl.t
  -> ?env:Persistent_env.t
  -> unit
  -> 'a

(** [run_elab comp ~raise ~options ~loc ~env] runs and elaborates the computation [comp] with the handler
    provided by [~raise], compiler options [~options] and initial environment [~env]. *)
val run_elab
  :  ('a Elaboration.t, Errors.typer_error, Main_warnings.all) t
  -> raise:(Errors.typer_error, Main_warnings.all) Trace.raise
  -> options:Compiler_options.middle_end
  -> loc:Location.t
  -> path:Module_var.t list
  -> ?env:Persistent_env.t
  -> unit
  -> 'a

(** [lift_elab comp] embeds the elaboration [comp] into the computation monad. *)
val lift_elab : 'a Elaboration.t -> ('a, Errors.typer_error, Main_warnings.all) t

(** {11 Error Recovery} *)

(** This section defines the [Error_recovery] module, whose functions define helpers to
    allow the typer to recover from failures, used by the language server. *)

(** Helpers to handle typer error recovery. *)
module Error_recovery : sig
  (** Checks whether the typer error recovery is enabled. *)
  val is_enabled : (bool, 'err, 'wrn) t

  (** Checks whether the typer error recovery is enabled, and if it is, logs the error
      from [error] and returns [default], otherwise raises the error from [error]. *)
  val raise_or_use_default
    :  error:'err Errors.with_loc
    -> default:('a, 'err, 'wrn) t
    -> ('a, 'err, 'wrn) t

  (** If the provided value is [None], logs the [error] and returns [default] (if error
      recovery is enabled), or raises [error] (otherwise). If the value is [Some], then it
      just returns the value without further actions. *)
  val raise_or_use_default_opt
    :  error:'err Errors.with_loc
    -> default:('a, 'err, 'wrn) t
    -> 'a option
    -> ('a, 'err, 'wrn) t

  (** If the provided value is [Error], logs the error and returns [default] (if error
      recovery is enabled), or raises the error (otherwise). If the value is [Ok], then it
      just calls [ok] on the value without further actions. *)
  val raise_or_use_default_result
    :  ok:('ok_result -> ('a, 'err, 'wrn) t)
    -> error:('err_result -> 'err) Errors.with_loc
    -> default:('a, 'err, 'wrn) t
    -> ('ok_result, 'err_result) result
    -> ('a, 'err, 'wrn) t

  (** If we could not infer some type, emit a placeholder type so typing can continue. *)
  val wildcard_type : (Type.t, 'err, 'wrn) t

  (** Checks whether the typer error recovery is enabled, and if its, returns [type],
      otherwise runs [on_disabled] (useful to throw some error). *)
  val raise_or_use_default_type : error:'err Errors.with_loc -> (Type.t, 'err, 'wrn) t

  (** If we could not infer some row, emit a placeholder type so typing can continue. *)
  val row : ('a Type.Row.t, 'err, 'wrn) t

  (** If we could not infer some signature, emit a placeholder type so typing can
      continue. *)
  val sig_ : (Context.Signature.t, 'err, 'wrn) t

  (** Utilities for getting values from the context, or dealing with error recovery in
      case they weren't found. *)
  module Get : sig
    val value
      :  Value_var.t
      -> error:([ `Mut_var_captured | `Not_found ] -> 'err Errors.with_loc)
      -> (Context_.mutable_flag * Type.t * Context_.Attrs.Value.t, 'err, 'wrn) t

    val imm
      :  Value_var.t
      -> error:'err Errors.with_loc
      -> (Type.t * Context_.Attrs.Value.t, 'err, 'wrn) t

    val mut
      :  Value_var.t
      -> error:([ `Mut_var_captured | `Not_found ] -> 'err Errors.with_loc)
      -> (Type.t, 'err, 'wrn) t

    val type_or_type_var
      :  Type_var.t
      -> error:'err Errors.with_loc
      -> ([ `Type of Type.t | `Type_var of Kind.t ], 'err, 'wrn) t

    val type_var : Type_var.t -> error:'err Errors.with_loc -> (Kind.t, 'err, 'wrn) t
    val type_ : Type_var.t -> error:'err Errors.with_loc -> (Type.t, 'err, 'wrn) t

    val module_
      :  Module_var.t
      -> error:'err Errors.with_loc
      -> (Context.Signature.t, 'err, 'wrn) t

    val module_of_path
      :  Module_var.t Nonempty_list.t
      -> error:'err Errors.with_loc
      -> (Context.Signature.t, 'err, 'wrn) t

    val module_type_of_path
      :  Module_var.t Nonempty_list.t
      -> error:'err Errors.with_loc
      -> (Context.Signature.t, 'err, 'wrn) t
  end
end

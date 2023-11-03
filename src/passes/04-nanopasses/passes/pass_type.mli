(* raise type for all nanopasses *)
type raise_t = (Errors.t, Main_warnings.all) Simple_utils.Trace.raise

(* [pass_kind] represents morphism over unified AST *)
type pass_kind = Morphing.pass_kind =
  | Seq : pass_kind * pass_kind -> pass_kind
  | Ignore : pass_kind -> pass_kind
  (*
    [Fold] for a catamorphism: leaf-to-root fold over unified AST.
    it accepts f-algebras (on all unified AST sorts)
  *)
  | Fold : Ast_unified.Catamorphism.idle_fold -> pass_kind
  (*
    [Refold_acc] for a hylomorphism:
    - first a leaf-to-root fold with accumulator
    - then roof-to-leaf unfold with accumlulator
    it accepts f-algebra/coalgebra's for both fold and unfold
  *)
  | Refold_acc : 'a Morphing.pass_fold * 'a Morphing.pass_unfold -> pass_kind
  (* [Check] for a iteration over unified AST *)
  | Check : Ast_unified.Iter.iter -> pass_kind
  (* [Nothing] do not apply any transformation *)
  | Nothing : pass_kind

(* [idle_fold] default idle f-alegbras for [Fold] *)
val idle_fold : Ast_unified.Catamorphism.idle_fold

(*
  [default_refold_acc] provides default f-algebra/coalgebra's for [Refold_acc]:
  for any type a, accepts a ~plus binary op on 'a and an initial value of type 'a
*)
val default_refold_acc
  :  plus:('a -> 'a -> 'a)
  -> init:'a
  -> 'a Morphing.pass_fold * 'a Morphing.pass_unfold

(* nanopasses are written into separated modules of type [T] *)
module type T = sig
  (* nanopass accepts argument of type [flag_arg] *)
  type flag_arg

  (*
    [flag] stores flags as local references.
    They are built from a boolean indicating whether the pass is enabled
    and a value of type [flag_arg].
    Note that all the pass are enabled/disabled before executing
  *)
  val flag : (bool * flag_arg) option Caml.ref

  (* [set_flag] set the [flag] reference with provided value *)
  val set_flag : enable:bool -> flag_arg -> unit

  (* [is_enabled] must return true when the pass is enabled *)
  val is_enabled : unit -> bool

  (* name of the pass as string (please use ocaml __MODULE__) *)
  val name : string

  (*
    reduction must throw an error if any unified AST node should never
    exist after the current pass
  *)
  val reduction : raise:raise_t -> Ast_unified.Iter.iter

  (* decompilecompile are function from the raise type to [pass_kind] *)
  val compile : raise:raise_t -> pass_kind
  val decompile : raise:raise_t -> pass_kind
end

(* [Selector] modules help you chose on what sort you want to compile or decompile *)
module Selector : sig
  type 'a t

  val expr : Ast_unified.expr t
  val program : Ast_unified.program t
  val block : Ast_unified.block t
  val pattern : Ast_unified.pattern t
  val ty_expr : Ast_unified.ty_expr t
  val declaration : Ast_unified.declaration t
  val instruction : Ast_unified.instruction t
  val sig_expr : Ast_unified.sig_expr t
end

(* [decompile_passes] decompile any value of of type 'a of the sort [sort] stoping
   before pass [stop_before] if specified *)
val decompile_passes
  :  raise:raise_t
  -> ?stop_before:string
  -> sort:'a Selector.t
  -> (module T) list
  -> 'a
  -> 'a

(* [decompile_passes] compile any value of of type 'a of the sort [sort] stoping
  before pass [stop_before] if specified *)
val compile_passes
  :  raise:raise_t
  -> ?stop_before:string
  -> sort:'a Selector.t
  -> (module T) list
  -> 'a
  -> 'a

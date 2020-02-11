open Trace

module I = Ast_simplified
module O = Ast_typed

module Environment = O.Environment

module Solver : module type of Typer_new.Solver

type environment = Environment.t

module Errors : sig
  (*
  val unbound_type_variable : environment -> string -> unit -> error
  val unbound_variable : environment -> string -> Location.t -> unit -> error
  val match_empty_variant : 'a I.matching -> Location.t -> unit -> error
  val match_missing_case : 'a I.matching -> Location.t -> unit -> error
  val match_redundant_case : 'a I.matching -> Location.t -> unit -> error
  val unbound_constructor : environment -> string -> Location.t -> unit -> error
  val unrecognized_constant : string -> Location.t -> unit -> error
  *)
  val wrong_arity : string -> int -> int -> Location.t -> unit -> error
  (*
  val match_tuple_wrong_arity : 'a list -> 'b list -> Location.t -> unit -> error

  (* TODO: this should be a trace_info? *)
  val program_error : I.program -> unit -> error
  val constant_declaration_error : string -> I.expr -> O.type_value option -> unit -> error
  val match_error : ?msg:string -> expected:'a I.matching -> actual:O.type_value -> Location.t -> unit -> error
  val needs_annotation : I.expression -> string -> unit -> error
  val type_error_approximate : ?msg:string -> expected:string -> actual:O.type_value -> expression:I.expression -> Location.t -> unit -> error
  val type_error : ?msg:string -> expected:O.type_value -> actual:O.type_value -> expression:I.expression -> Location.t -> unit -> error
  val bad_tuple_index : int -> I.expression -> O.type_value -> Location.t -> unit -> error
  val bad_record_access : string -> I.expression -> O.type_value -> Location.t -> unit -> error
  val not_supported_yet : string -> I.expression -> unit -> error
  val not_supported_yet_untranspile : string -> O.expression -> unit -> error
  val constant_error : Location.t -> O.type_value list -> O.type_value option -> unit -> error
  *)
end

val type_program : I.program -> (O.program * Solver.state) result
val type_declaration : environment -> Solver.state -> I.declaration -> (environment * Solver.state * O.declaration option) result
(* val type_match : (environment -> 'i -> 'o result) -> environment -> O.type_value -> 'i I.matching -> I.expression -> Location.t -> 'o O.matching result *)
val evaluate_type : environment -> I.type_expression -> O.type_expression result
val type_expression : environment -> Solver.state -> ?tv_opt:O.type_expression -> I.expression -> (O.expression * Solver.state) result
val type_constant : I.constant' -> O.type_expression list -> O.type_expression option -> (O.constant' * O.type_expression) result
(*
val untype_type_value : O.type_value -> (I.type_expression) result
val untype_literal : O.literal -> I.literal result
*)
val untype_expression : O.expression -> I.expression result
(*
val untype_matching : ('o -> 'i result) -> 'o O.matching -> ('i I.matching) result
*)

open Trace
open Ast_simplified

module Raw = Parser.Pascaligo.AST
module SMap = Map.String

(*
val nseq_to_list : 'a * 'a list -> 'a list
val npseq_to_list : 'a * ( 'b * 'a ) list -> 'a list
*)
val npseq_to_nelist : 'a * ( 'b * 'c ) list -> 'a * 'c list
(*
val pseq_to_list : ('a * ('b * 'a) list) option -> 'a list
val get_value : 'a Raw.reg -> 'a
*)

module Errors : sig
  (*
  val unsupported_cst_constr : Raw.pattern -> unit -> error
  val unsupported_ass_None : Raw.wild -> unit -> error
  val unsupported_entry_decl : 'a Raw.reg -> unit -> error
  val unsupported_proc_decl : 'a Raw.reg -> unit -> error
  *)
  val bad_bytes : Location.t -> string -> unit -> error
  (*
  val unsupported_local_proc : Raw.wild -> unit -> error
  val corner_case : loc:string -> string -> unit -> error
  val unknown_predefined_type : string Raw.reg -> unit -> error
  *)
  val unsupported_arith_op : Raw.expr -> unit -> error
  (*
  val unsupported_set_expr : Raw.expr -> unit -> error
  *)
  val unsupported_proc_calls : 'a Raw.reg -> unit -> error
  (*
  val unsupported_for_loops : Raw.wild -> unit -> error
  val unsupported_deep_map_assign : 'a Raw.reg -> unit -> error
  val unsupported_empty_record_patch : 'a Raw.reg -> unit -> error
  val unsupported_map_patches : 'a Raw.reg -> unit -> error
  val unsupported_set_patches : 'a Raw.reg -> unit -> error
  val unsupported_deep_map_rm : 'a Raw.reg -> unit -> error
  val unsupported_set_removal : 'a Raw.reg -> unit -> error
  val unsupported_non_var_pattern : Raw.pattern -> unit -> error
  val only_constructors : Raw.pattern -> unit -> error
  val unsupported_tuple_pattern : Raw.pattern -> unit -> error
  val unsupported_deep_Some_patterns : Raw.pattern -> unit -> error
  val unsupported_deep_list_patterns : 'a Raw.reg -> unit -> error
  val unsupported_sub_blocks : 'a Raw.reg -> unit -> error
  val simplifying_instruction : Raw.instruction -> unit -> error
  *)
end

(*
val r_split : 'a Raw.reg -> 'a * Location.t
val return : expr -> ( expr option -> expr result ) result
val return_let_in : ?loc:Location.t -> string * type_expression option -> expr -> ( expr option -> expr result ) result
val simpl_type_expression : Raw.type_expr -> type_expression result
val simpl_list_type_expression : Raw.type_expr list -> type_expression result
*)
val simpl_expression : Raw.expr -> expr result
(*
val simpl_logic_expression : Raw.logic_expr -> expression result
val simpl_list_expression : Raw.list_expr -> expression result
val simpl_set_expression : Raw.set_expr -> expression result
val simpl_binop : string -> Raw.wild Raw.bin_op Region.reg -> expression result
val simpl_unop : string -> Raw.wild Raw.un_op Region.reg -> expression result
val simpl_tuple_expression : ?loc:Location.t -> Raw.expr list -> expression result
val simpl_local_declaration : Raw.local_decl -> ( expr option -> expr result) result
val simpl_data_declaration : Raw.data_decl -> ( expr option -> expr result ) result
val simpl_param : Raw.param_decl -> (type_name * type_expression) result
val simpl_fun_declaration : loc:Location.t -> Raw.fun_decl -> ((name * type_expression option) * expression) result
val simpl_declaration : Raw.declaration -> declaration Location.wrap result
val simpl_single_instruction : Raw.single_instr -> (expression option -> expression result) result
val simpl_path : Raw.path -> string * Ast_simplified.access_path
val simpl_cases : (Raw.pattern * 'a) list -> 'a matching result
val simpl_instruction_block : Raw.instruction -> (expression option -> expression result) result
val simpl_instruction : Raw.instruction -> (expression option -> expression result) result
val simpl_statements : Raw.statements -> (expression option -> expression result) result
val simpl_block : Raw.block -> (expression option -> expression result) result
*)
val simpl_program : Raw.ast -> program result

[@@@warning "-45"]

open Trace

open Ast_simplified

module Raw = Parser.Cameligo.AST
module SMap = Map.String
module Option = Simple_utils.Option

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
  val wrong_pattern : string -> Raw.pattern -> unit -> error
  val multiple_patterns : string -> Raw.pattern list -> unit -> error
  val unknown_predefined_type : string Raw.reg -> unit -> error
  val unsupported_arith_op : Raw.expr -> unit -> error
  val unsupported_string_catenation : Raw.expr -> unit -> error
  val untyped_fun_param : 'a Raw.reg -> unit -> error
  val unsupported_tuple_pattern : Raw.pattern -> unit -> error
  val unsupported_cst_constr : Raw.pattern -> unit -> error
  val unsupported_non_var_pattern : Raw.pattern -> unit -> error
  val simplifying_expr : Raw.expr -> unit -> error
  val only_constructors : Raw.pattern -> unit -> error
  val unsupported_sugared_lists : Raw.wild -> unit -> error
  val bad_set_definition : unit -> error
  val bad_list_definition : unit -> error
  val bad_map_definition : unit -> error
  val corner_case : loc:string -> string -> unit -> error
  *)
end


(*
val r_split : 'a Raw.reg -> 'a * Location.t
val pattern_to_var : Raw.pattern -> Raw.variable result
val pattern_to_typed_var : Raw.pattern -> ( Raw.variable * Raw.type_expr option ) result
val expr_to_typed_expr : Raw.expr -> ( Raw.expr * Raw.type_expr option ) result
val patterns_to_var : Raw.pattern list -> Raw.variable result
val simpl_type_expression : Raw.type_expr -> type_expression result
val simpl_list_type_expression : Raw.type_expr list -> type_expression result
*)
val simpl_expression : Raw.expr -> expr result
(*
val simpl_fun : Raw.fun_expr Raw.reg -> expr result 
val simpl_logic_expression : ?te_annot:type_expression -> Raw.logic_expr -> expr result
val simpl_list_expression : Raw.list_expr -> expression result
val simpl_binop : string -> Raw.wild Raw.bin_op Region.reg -> expression result 
val simpl_unop : string -> Raw.wild Raw.un_op Region.reg -> expression result
val simpl_tuple_expression : ?loc:Location.t -> Raw.expr list -> expression result
val simpl_declaration : Raw.declaration -> declaration Location.wrap result
val simpl_cases : (Raw.pattern * 'a) list -> 'a matching result
*)
val simpl_program : Raw.ast -> program result

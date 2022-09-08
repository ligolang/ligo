module AST = Ast_aggregated
module Append_tree = Simple_utils.Tree.Append
open Simple_utils.Trace
open Ligo_interpreter.Types
open Ligo_prim

let extract_record ~raise ~(layout:Layout.t) (v : value) (lst : (Label.t * AST.type_expression) list) : _ list =
  trace ~raise Main_errors.spilling_tracer @@ Spilling.Layout.extract_record ~layout v lst Ligo_interpreter.Combinators.get_pair

let extract_constructor ~raise ~(layout:Layout.t) (v : value) (lst : (Label.t * AST.type_expression) list) : (Label.t * value * AST.type_expression) =
  trace ~raise Main_errors.spilling_tracer @@ Spilling.Layout.extract_constructor ~layout v lst Ligo_interpreter.Combinators.get_left Ligo_interpreter.Combinators.get_right

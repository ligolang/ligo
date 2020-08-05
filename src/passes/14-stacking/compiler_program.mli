open Errors
open Trace
open Co_de_bruijn
open Co_de_bruijn.Util

open Michelson
open Memory_proto_alpha.Protocol.Script_ir_translator

(*
module Contract_types = Meta_michelson.Types
module Stack = Meta_michelson.Stack
*)
type compiled_expression = {
  expr_ty : ex_ty ;
  expr : michelson ;
}


val translate_expression : expression -> environment -> splitting -> (michelson, stacking_error) result

val translate_function_body : expression bind -> environment -> splitting -> (michelson, stacking_error) result

val translate_value : value -> type_expression -> (michelson, stacking_error) result

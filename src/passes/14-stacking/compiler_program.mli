open Errors
open Trace
open! Co_de_bruijn
open Co_de_bruijn.Util

open Michelson

(*
module Contract_types = Meta_michelson.Types
module Stack = Meta_michelson.Stack
*)
type compiled_expression = {
  expr_ty : michelson ;
  expr : michelson ;
}


val translate_expression : expression -> environment -> splitting -> (michelson, stacking_error) result

val translate_function_body : expression bind -> environment -> splitting -> (michelson, stacking_error) result

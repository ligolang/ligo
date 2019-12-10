open Trace
open Mini_c

open Michelson
open Memory_proto_alpha.Protocol.Script_ir_translator
open Operators.Compiler

(*
module Contract_types = Meta_michelson.Types
module Stack = Meta_michelson.Stack
*)
type compiled_expression = {
  expr_ty : ex_ty ;
  expr : michelson ;
}

val get_operator : constant -> type_value -> expression list -> predicate result
val translate_expression : expression -> environment -> michelson result
val translate_function_body : anon_function -> environment_element list -> type_value -> michelson result
val translate_value : value -> type_value -> michelson result 

(*

open Operators.Compiler

val get_predicate : string -> type_value -> expression list -> predicate result

val translate_function : anon_function -> michelson result

val translate_expression : ?push_var_name:string -> expression -> environment -> ( michelson * environment ) result

val translate_quote_body : anon_function -> michelson result

val get_main : program -> string -> anon_function result


module Errors : sig
  val corner_case : loc:string -> string -> unit -> error
end

*)

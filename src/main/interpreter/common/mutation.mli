(* Virtual module. Will be linked in later *)

module LT = Ligo_interpreter.Types
module LC = Ligo_interpreter.Combinators

val mutate_some_value
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?syntax:Syntax_types.t
  -> LT.Location.t
  -> LT.Z.t
  -> LT.value
  -> LT.type_expression
  -> (LT.expression * LT.mutation) option

val mutate_all_value
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?syntax:Syntax_types.t
  -> LT.Location.t
  -> LT.value
  -> LT.type_expression
  -> (LT.expression * LT.mutation) list

val value_gen
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?small:bool
  -> ?known_addresses:LT.Contract.t list
  -> LT.type_expression
  -> LT.value QCheck.Gen.t

val mutate_some_contract
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?syntax:Syntax_types.t
  -> LT.Z.t
  -> LT.expression
  -> (LT.expression * LT.mutation) option

val buffer_of_mutation : 'a -> 'b
val get_mutation_id : 'a -> 'b

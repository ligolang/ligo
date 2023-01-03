(* Virtual module. Will be linked in later *)

val mutate_some_value
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?syntax:Syntax_types.t
  -> Ligo_interpreter.Types.Location.t
  -> Ligo_interpreter.Types.Z.t
  -> Ligo_interpreter.Types.value
  -> Ligo_interpreter.Types.type_expression
  -> (Ligo_interpreter.Types.expression * Ligo_interpreter.Types.mutation) option

val mutate_all_value
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?syntax:Syntax_types.t
  -> Ligo_interpreter.Types.Location.t
  -> Ligo_interpreter.Types.value
  -> Ligo_interpreter.Types.type_expression
  -> (Ligo_interpreter.Types.expression * Ligo_interpreter.Types.mutation) list

val value_gen
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?small:bool
  -> ?known_addresses:Ligo_interpreter.Types.Contract.t list
  -> Ligo_interpreter.Types.type_expression
  -> Ligo_interpreter.Types.value QCheck.Gen.t

val mutate_some_contract
  :  raise:(Errors.interpreter_error, 'a) Simple_utils.Trace.raise
  -> ?syntax:Syntax_types.t
  -> Ligo_interpreter.Types.Z.t
  -> Ligo_interpreter.Types.expression
  -> (Ligo_interpreter.Types.expression * Ligo_interpreter.Types.mutation) option

val buffer_of_mutation : 'a -> 'b
val get_mutation_id : 'a -> 'b

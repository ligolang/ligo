open Errors
open Mini_c.Types
open Simple_utils.Trace

val decompile_value :
  raise: stacking_error raise ->
  'l Michelson.t -> 'l Michelson.t ->
  value

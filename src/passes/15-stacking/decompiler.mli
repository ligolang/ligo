open Errors
open Mini_c.Types
open Trace

val decompile_value :
  'l Michelson.t -> 'l Michelson.t ->
  (value, stacking_error) result

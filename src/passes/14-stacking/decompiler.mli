open Errors
open Mini_c.Types
open Trace

val decompile_value :
  Michelson.t -> Michelson.t ->
  (value, stacking_error) result

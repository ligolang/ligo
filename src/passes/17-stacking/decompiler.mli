open Mini_c.Types
module Trace = Simple_utils.Trace

val decompile_value
  :  raise:(Errors.stacking_error, _) Trace.raise
  -> 'l Errors.Michelson.t
  -> 'l Errors.Michelson.t
  -> value

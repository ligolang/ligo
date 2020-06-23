open Errors
open Mini_c.Types
open Proto_alpha_utils.Memory_proto_alpha
open X
open Proto_alpha_utils.Trace

val decompile_value : ex_typed_value -> (value , stacking_error) result

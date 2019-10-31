open Trace

val error_pp : ?dev:bool -> Format.formatter -> error -> unit

val result_pp_hr : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Simple_utils.Trace.result -> unit

val string_result_pp_hr : Format.formatter -> string Simple_utils.Trace.result -> unit


val result_pp_dev : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a Simple_utils.Trace.result -> unit

val string_result_pp_dev : Format.formatter -> string Simple_utils.Trace.result -> unit

val json_pp : Format.formatter -> Simple_utils.Trace.J.t -> unit

val string_result_pp_json : Format.formatter -> string result -> unit

type display_format = [
  | `Human_readable
  | `Json
  | `Dev
]

val formatted_string_result_pp : display_format -> Format.formatter -> string Simple_utils.Trace.result -> unit

type michelson_format = [
  | `Text
  | `Json
  | `Hex
]

val michelson_pp : michelson_format -> Format.formatter -> Tezos_utils.Michelson.michelson -> unit

(* Parsing errors for the compiler *)

val stage : string

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module Display  = Simple_utils.Display

(* Errors *)

type t = [`Parsing of string Region.reg]

type error = t

(* Colour snippet (Format) *)

type pp_formater =
  display_format:(string Display.display_format) ->
  no_colour:bool ->
  Format.formatter ->
  t ->
  unit

val error_ppformat : pp_formater
val to_ppformat    : pp_formater (* Alias of [error_ppformat] *)

(* JSON *)

val error_json : t -> Simple_utils.Error.t

module ErrorPrefix : sig
  val add : string -> string
  val remove : string -> string
  val is_contained : string -> bool
end

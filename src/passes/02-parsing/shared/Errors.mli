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

module ErrorWrapper : sig
  val wrap : string -> string
  val unwrap : string -> string
  (* The first arg is a replacement, the second one is a string *)
  val replace_with : string -> string -> string
  val is_wrapped : string -> bool
end

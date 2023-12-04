module LigoRC = Ligo_rc
module LigoIgnore = Ligo_ignore

type unzip_error = UnableToUnzip

val unzip : string -> (string, unzip_error) result
val untar : dest_dir:string -> string -> unit

module Checksum : sig
  type error = IntegrityMismatch

  val string_of_error : error -> string
  val sha1_bytes : bytes -> string
  val sha1 : string -> string
  val check_integrity : string -> expected:string -> (unit, error) result
end

val find_project_root : unit -> string option

type return =
  | Done
  | Compileur_Error
  | Exception of exn

val return_with_custom_formatter
  :  cli_analytics:Analytics.analytics_input list
  -> skip_analytics:bool
  -> return:return ref
  -> ?show_warnings:bool
  -> ?output_file:string
  -> (unit -> (string * string, string * string) result)
  -> unit

val return_result_lwt
  :  cli_analytics:Analytics.analytics_inputs
  -> skip_analytics:bool
  -> return:return ref
  -> ?show_warnings:bool
  -> ?output_file:string
  -> ?minify_json:bool
  -> display_format:Simple_utils.Display.ex_display_format
  -> no_colour:bool
  -> warning_as_error:bool
  -> 'value Simple_utils.Display.format
     * (raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
        -> ('value * Analytics.analytics_inputs) Lwt.t)
  -> unit

val return_result
  :  cli_analytics:Analytics.analytics_inputs
  -> skip_analytics:bool
  -> return:return ref
  -> ?show_warnings:bool
  -> ?output_file:string
  -> ?minify_json:bool
  -> display_format:Simple_utils.Display.ex_display_format
  -> no_colour:bool
  -> warning_as_error:bool
  -> 'value Simple_utils.Display.format
     * (raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
        -> 'value * Analytics.analytics_inputs)
  -> unit

module LigoRC = Ligo_rc
module LigoManifest = Ligo_manifest
module LigoIgnore = Ligo_ignore
module RepositoryUrl = Repository_url
module Constants = Constants
module Semver = LigoManifest.Semver

val find_project_root : unit -> string option

type return =
  | Done
  | Compileur_Error
  | Exception of exn

type command = string * string array

(* Checks if executable is present *)
val does_command_exist : string -> (bool, string) result

val return_with_custom_formatter
  :  cli_analytics:Analytics.analytics_input list
  -> skip_analytics:bool
  -> return:return ref
  -> ?show_warnings:bool
  -> ?output_file:string
  -> (unit -> (string * string, string * string) result)
  -> unit

val return_result
  :  cli_analytics:Analytics.analytics_inputs
  -> skip_analytics:bool
  -> return:return ref
  -> ?show_warnings:bool
  -> ?output_file:string
  -> display_format:Simple_utils.Display.ex_display_format
  -> no_colour:bool
  -> warning_as_error:bool
  -> 'value Simple_utils.Display.format
     * (raise:(Main_errors.all, Main_warnings.all) Simple_utils.Trace.raise
        -> 'value * Analytics.analytics_inputs)
  -> unit

(* Runs a commands in a separate process *)
val run_command : command -> (unit, string) result

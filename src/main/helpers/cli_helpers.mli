module LigoRC = Ligo_rc
module LigoManifest = Ligo_manifest
module RepositoryUrl = Repository_url

module Constants : sig
  type command = string * string array

  val esy : string
  val ligo_install_path : string
  val ligo_rc_path : string
  val ligo_registry : string

  val esy_add
    :  package_name:string
    -> cache_path:string
    -> ligo_registry:string
    -> command

  val esy_install : cache_path:string -> ligo_registry:string -> command
  val git_clone : project_url:string -> project_name:string -> command
  val git_checkout : dir_path:string -> ref:string -> command

  val ligo_compile_storage
    :  ?ligo:string
    -> main:string
    -> expression:string
    -> unit
    -> command
end

val find_project_root : unit -> string option

type return =
  | Done
  | Compileur_Error
  | Exception of exn

val return_result
  :  return:return ref
  -> ?show_warnings:bool
  -> ?output_file:string
  -> (unit -> (string * string, string * string) result)
  -> unit

type command = string * string array

(* Checks if executable is present *)
val does_command_exist : string -> (bool, string) result

(* Runs a commands in a separate process *)
val run_command : command -> (unit, string) result

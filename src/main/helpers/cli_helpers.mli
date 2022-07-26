module Constants : sig 
  type command = (string * string array)
  val esy : string
  val ligo_install_path : string
  val ligo_rc_path : string
  val ligo_registry : string
  val esy_add : package_name:string -> cache_path:string -> ligo_registry:string -> command
  val esy_install : cache_path:string -> ligo_registry:string -> command
  val git_clone : project_url:string  -> project_name:string  -> command
  val git_checkout : dir_path:string  -> ref:string  -> command
end

module LigoRC : sig
  type t
  val get_token : registry_key:string -> t -> string option
  val read : ligorc_path:string -> t
  val update_token : registry_key:string -> token:string -> t -> t
  val write : t -> unit
  val registry_key : string -> string
end

module LigoManifest : sig
  type t =
    { name               : string
    ; version            : string
    ; description        : string
    ; scripts            : (string * string) list
    ; author             : string
    ; license            : string
    ; readme             : string
    ; ligo_manifest_path : string
    } [@@deriving to_yojson]
  
  val validate : t -> t

  val read : project_root:string option -> t
end

val find_project_root : unit -> string option

type return = Done | Compileur_Error | Exception of exn
val return_result : return:return ref -> ?show_warnings:bool -> ?output_file:string -> (unit -> (string*string,string*string) result) -> unit

type command = (string * string array)

(* Checks if executable is present *)
val does_command_exist : string -> (bool, string) result

(* Runs a commands in a separate process *)
val run_command : command -> (unit, string) result
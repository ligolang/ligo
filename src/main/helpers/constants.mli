type command = string * string array

val esy : string
val windows : string
val ligo_install_path : string
val ligo_rc_path : unit -> string
val ligo_registry : string
val esy_add : package_name:string -> cache_path:string -> ligo_registry:string -> command
val esy_install : cache_path:string -> ligo_registry:string -> command
val git_clone : project_url:string -> project_name:string -> command
val git_checkout : dir_path:string -> ref:string -> command
val where : cmd:string -> command
val which : cmd:string -> command

val ligo_compile_storage
  :  ?ligo:string
  -> main:string
  -> expression:string
  -> unit
  -> command

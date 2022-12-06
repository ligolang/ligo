type t

val get_token : registry_key:string -> t -> string option
val read : ligorc_path:string -> t
val update_token : registry_key:string -> token:string -> t -> t
val write : t -> unit
val registry_key : string -> string

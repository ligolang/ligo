(** [publish] makes a HTTP put request to the [ligo_registry] with the [body] 
    and used [token] for authorization *)

val publish
  :  ligo_registry:Uri.t
  -> ligorc_path:string
  -> project_root:string option
  -> dry_run:bool
  -> (string * string, string * string) result

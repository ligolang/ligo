(** Accomplishes unpublishing of a package by following the registry's API protocol. *)

(* TODO Move more logic inside [Registry] *)

val unpublish
  :  name:string option
  -> version:string option
  -> ligo_registry:Uri.t
  -> ligorc_path:string
  -> (string * string, string * string) result

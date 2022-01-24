(* Module for resolving paths to external libraies *)

type t

val make : string -> t option

val get_root_inclusion_list : t option -> string list

val get_inclusion_list : file:string -> t option -> string list

val find_external_file : file:string -> inclusion_list:string list -> string option

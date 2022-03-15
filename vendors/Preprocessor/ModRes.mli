(* Module for resolving paths to external libraies *)

type t

type inclusion_dir = [`Inclusion of string]
type inclusion_list = inclusion_dir list

val make : string -> t option

val get_root_inclusion_list : t option -> inclusion_list

val get_inclusion_list : file:string -> t option -> inclusion_list

val find_external_file : file:string -> inclusion_list:inclusion_list -> string option

val pp : Format.formatter -> t -> unit
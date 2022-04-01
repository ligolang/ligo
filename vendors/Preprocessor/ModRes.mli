(* Module for resolving paths to external libraies *)

type t

type dependency_path      = [`Path of string]
type dependency_path_list = dependency_path list

val make : string -> t option

val get_paths_of_root_dependencies : t option -> dependency_path_list

val get_paths_of_dependencies : file:string -> t option -> dependency_path_list

val find_external_file : file:string -> inclusion_list:dependency_path_list -> string option

val pp : Format.formatter -> t -> unit

module Helpers : sig

  val resolve_file_name : string -> t option -> string

end
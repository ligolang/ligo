(* Module for resolving paths to external libraries *)

type t

type dependency_path = Path of string

val equal_paths : dependency_path -> dependency_path -> bool

val make : string -> t option (* The parameter is the project root. *)

val get_root_dependencies :
  t option -> dependency_path list

val get_dependencies :
  file:string -> t option -> dependency_path list

val find_external_file :
  file:string -> inclusion_paths:dependency_path list -> string option

val pp : Format.formatter -> t -> unit

module Helpers :
  sig
    val resolve : file:string -> t option -> string
  end

(** A convenience module to handle the name-version tuple.

    We often find ourselves indexing or identifying a package with just it's name and version. This module comes handy as the argument for package Maps, cache Maps etc *)

open Package_management_external_libs
module Semver = Ligo_semver

type version =
  | SemverVersion of Semver.t
  | StringVersion of string

type t =
  { name : string
  ; version : version
  }

val compare : t -> t -> int
val make : name:string -> version:version -> t
val is_root : t -> bool
val version_of_string : string -> version option
val version_to_string : version -> string
val to_string : t -> string
val of_string : string -> (t, [> Rresult.R.msg ]) result
val to_yojson : t -> Yojson.Safe.t
val of_yojson : Yojson.Safe.t -> (t, string) result

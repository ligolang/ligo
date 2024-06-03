(** Memory representation of ligo manifest. *)

open Package_management_external_libs
module Semver = Ligo_semver

(* TODO Merge with [Ligo_manifest] *)

module DependencyMap : module type of String.Map

type t =
  { dependencies : Semver.t DependencyMap.t
  ; dev_dependencies : Semver.t DependencyMap.t [@key "devDependencies"]
  }

val of_yojson : Yojson.Safe.t -> (t, string) result
val to_yojson : t -> Yojson.Safe.t

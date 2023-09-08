(** Module dealing with ligo manifest (package.json) *)

open Package_management_external_libs
open Package_management_shared
module Semver = Ligo_semver

type t =
  { name : string
  ; version : Semver.t
  ; description : string
  ; scripts : (string * string) list
  ; dependencies : (string * string) list
  ; dev_dependencies : (string * string) list
  ; main : string
  ; author : string
  ; type_ : string
  ; storage_fn : string option
  ; storage_arg : string option
  ; repository : Repository_url.t
  ; license : string
  ; readme : string
  ; ligo_manifest_path : string
  ; bugs : Bugs.t
  }
[@@deriving yojson]

(** Returns validation errors if any, present in the project's ligo manifest. Note that, this doesn't include parsing error - this function takes a valid [t] that parsed correctly, but could contain bad semantics *)
val validate : t -> (unit, string) result

(** Returns a [t] for a project situated at [project_root] *)
val read : project_root:string option -> (t, string) result

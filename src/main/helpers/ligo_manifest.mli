module Bugs : sig
  type t [@@deriving yojson]
end

module Semver : sig
  include module type of Semver

  type t [@@deriving yojson]

  val to_string : t -> string
end

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

val validate : ligo_bin_path:string -> t -> (unit, string) result
val read : project_root:string option -> (t, string) result

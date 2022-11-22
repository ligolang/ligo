module Bugs : sig
  type t [@@deriving to_yojson]
end

type t =
  { name : string
  ; version : string
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
[@@deriving to_yojson]

val validate : t -> (t, string) result
val read : project_root:string option -> (t, string) result

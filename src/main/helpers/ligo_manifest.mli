type t =
  { name : string
  ; version : string
  ; description : string
  ; scripts : (string * string) list
  ; main : string option
  ; author : string
  ; contract: bool
  ; repository : Repository_url.t
  ; license : string
  ; readme : string
  ; ligo_manifest_path : string
  }
[@@deriving to_yojson]

val validate : t -> (t, string) result
val read : project_root:string option -> (t, string) result

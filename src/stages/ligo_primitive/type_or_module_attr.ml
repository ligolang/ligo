type t =
  { public : bool
  ; hidden : bool
  ; leading_comments : string list
  }
[@@deriving eq, compare, yojson, hash]

open Format
open Value_attr.PP_attributes

let pp ppf { public; hidden; leading_comments } =
  fprintf
    ppf
    "%a%a%a"
    (pp_if_set "private")
    (not public)
    (pp_if_set "hidden")
    hidden
    pp_comments
    leading_comments


let default_attributes = { public = true; hidden = false; leading_comments = [] }

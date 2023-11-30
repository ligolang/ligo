type t =
  { public : bool
  ; leading_comments : string list
  }
[@@deriving eq, compare, yojson, hash]

open Format
open Value_attr.PP_attributes

let pp ppf { public; leading_comments } =
  fprintf ppf "%a%a" (pp_if_set "private") (not public) pp_comments leading_comments


let default_attributes = { public = true; leading_comments = [] }

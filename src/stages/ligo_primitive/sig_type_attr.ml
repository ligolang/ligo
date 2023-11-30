type t = { leading_comments : string list } [@@deriving eq, compare, yojson, hash]

open Format
open Value_attr.PP_attributes

let pp ppf { leading_comments } = fprintf ppf "%a" pp_comments leading_comments
let default_attributes = { leading_comments = [] }

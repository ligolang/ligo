type t =
  { public : bool
  ; leading_comments : string list
  }
[@@deriving eq, compare, yojson, hash, bin_io]

open Format
open Value_attr.PP_attributes

let pp ppf { public; leading_comments } =
  fprintf ppf "%a%a" (pp_if_set "private") (not public) pp_comments leading_comments


let default_attributes = { public = true; leading_comments = [] }

let apply_sig_attr ~key ~value attr =
  match key, value with
  | "private", None -> `Ok { attr with public = false }
  | "public", None -> `Ok { attr with public = true }
  | "comment", Some comment ->
    `Ok { attr with leading_comments = comment :: attr.leading_comments }
  | _ -> `Invalid_attribute

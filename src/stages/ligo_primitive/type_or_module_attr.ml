type t =
  { public : bool
  ; hidden : bool
  ; leading_comments : string list
  ; deprecated : string option
  }
[@@deriving eq, compare, yojson, hash, bin_io]

open Format
open Value_attr.PP_attributes

let pp ppf { public; hidden; leading_comments; deprecated } =
  fprintf
    ppf
    "%a%a%a%a"
    (pp_if_set "private")
    (not public)
    (pp_if_set "hidden")
    hidden
    (pp_if_some "deprecated")
    deprecated
    pp_comments
    leading_comments


let default_attributes =
  { public = true; hidden = false; leading_comments = []; deprecated = None }


let apply_mod_or_sig ~key ~value attr =
  match key, value with
  | "private", None -> `Ok { attr with public = false }
  | "public", None -> `Ok { attr with public = true }
  | "hidden", None -> `Ok { attr with hidden = true }
  | "comment", Some comment ->
    `Ok { attr with leading_comments = comment :: attr.leading_comments }
  | "deprecated", value -> `Ok { attr with deprecated = value }
  | _ -> `Invalid_attribute

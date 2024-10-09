type t =
  { entry : bool
  ; view : bool
  ; dyn_entry : bool
  ; optional : bool
  ; leading_comments : string list
  }
[@@deriving eq, compare, yojson, hash, bin_io]

open Format
open Value_attr.PP_attributes

let pp ppf { view; entry; dyn_entry; optional; leading_comments } =
  fprintf
    ppf
    "%a%a%a%a%a"
    (pp_if_set "view")
    view
    (pp_if_set "entry")
    entry
    (pp_if_set "dyn_entry")
    dyn_entry
    (pp_if_set "optional")
    optional
    pp_comments
    leading_comments


let default_attributes =
  { entry = false
  ; view = false
  ; dyn_entry = false
  ; optional = false
  ; leading_comments = []
  }


let apply_sig_item_attr ~key ~value attr =
  match key, value with
  | "view", None -> `Ok { attr with view = true }
  | "entry", None -> `Ok { attr with entry = true }
  | "dyn_entry", None -> `Ok { attr with dyn_entry = true }
  | "comment", Some comment ->
    `Ok { attr with leading_comments = comment :: attr.leading_comments }
  | _ -> `Invalid_attribute

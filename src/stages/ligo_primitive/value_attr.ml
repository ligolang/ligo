type t =
  { inline : bool
  ; no_mutation : bool
  ; (* Some external constant (e.g. `Test.balance`) do not accept any argument. This annotation is used to prevent LIGO interpreter to evaluate (V_Thunk values) and forces inlining in the compiling (15-self_mini_c)
  TODO: we should change the type of such constants to be `unit -> 'a` instead of just 'a
*)
    view : bool
  ; entry : bool
  ; dyn_entry : bool
  ; public : bool
  ; (* Controls whether a declaration must be printed or not when using LIGO print commands (print ast-typed , ast-aggregated .. etc ..)
  set to true for standard libraries
*)
    hidden : bool
  ; (* Controls whether it should be inlined at AST level *)
    thunk : bool
  ; tzip16_compatible : bool
  ; (* No support from compiler at the moment, but acknowledged by LSP *)
    deprecated : string option
  ; leading_comments : string list
  }
[@@deriving eq, compare, yojson, hash, bin_io]

open Format

module PP_attributes = struct
  let pp_if_set str ppf attr = if attr then fprintf ppf "[@@%s]" str else fprintf ppf ""

  let pp_if_some str ppf attr =
    if Option.is_some attr
    then fprintf ppf "[@@%s %s]" str (Option.value_exn attr)
    else fprintf ppf ""


  let pp_comments ppf comments =
    List.iter comments ~f:(fun comment -> pp_if_some "comment" ppf (Some comment))
end

open PP_attributes

let pp
    ppf
    { inline
    ; no_mutation
    ; view
    ; entry
    ; dyn_entry
    ; public
    ; hidden
    ; thunk
    ; tzip16_compatible
    ; deprecated
    ; leading_comments
    }
  =
  fprintf
    ppf
    "%a%a%a%a%a%a%a%a%a%a%a"
    (pp_if_set "inline")
    inline
    (pp_if_set "no_mutation")
    no_mutation
    (pp_if_set "view")
    view
    (pp_if_set "entry")
    entry
    (pp_if_set "dyn_entry")
    dyn_entry
    (pp_if_set "private")
    (not public)
    (pp_if_set "hidden")
    hidden
    (pp_if_set "thunk")
    thunk
    (pp_if_set "tzip16_compatible")
    tzip16_compatible
    (pp_if_some "deprecated")
    deprecated
    pp_comments
    leading_comments


let default_attributes =
  { inline = false
  ; no_mutation = false
  ; view = false
  ; entry = false
  ; public = true
  ; hidden = false
  ; thunk = false
  ; tzip16_compatible = false
  ; dyn_entry = false
  ; deprecated = None
  ; leading_comments = []
  }


let apply_decl_attr ~key ~value attr =
  match key, value with
  | "inline", None -> `Ok { attr with inline = true }
  | "no_mutation", None -> `Ok { attr with no_mutation = true }
  | "view", None -> `Ok { attr with view = true }
  | "private", None -> `Ok { attr with public = false }
  | "public", None -> `Ok { attr with public = true }
  | "hidden", None -> `Ok { attr with hidden = true }
  | "thunk", None -> `Ok { attr with thunk = true }
  | "tzip16_compatible", None -> `Ok { attr with tzip16_compatible = true }
  | "entry", None -> `Ok { attr with entry = true }
  | "comment", Some comment ->
    `Ok { attr with leading_comments = comment :: attr.leading_comments }
  | "dyn_entry", None -> `Ok { attr with dyn_entry = true }
  | "deprecated", value -> `Ok { attr with deprecated = value }
  | _ -> `Invalid_attribute


let apply_expr_attr ~key ~value attr =
  (* TODO: more granual failure *)
  match key, value with
  | "inline", None -> `Ok { attr with inline = true }
  | "no_mutation", None -> `Ok { attr with no_mutation = true }
  | "thunk", None -> `Ok { attr with thunk = true }
  | "tzip16_compatible", None -> `Ok { attr with tzip16_compatible = true }
  | "private", None -> `Ok { attr with public = false }
  | "public", None -> `Ok { attr with public = true }
  | "comment", Some comment ->
    `Ok { attr with leading_comments = comment :: attr.leading_comments }
  | "deprecated", value -> `Ok { attr with deprecated = value }
  | _ -> `Invalid_attribute

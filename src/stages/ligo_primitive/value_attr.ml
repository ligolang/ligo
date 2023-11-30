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
  ; deprecated : string option
  ; leading_comments : string list
  }
[@@deriving eq, compare, yojson, hash]

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
    ; deprecated
    ; leading_comments
    }
  =
  fprintf
    ppf
    "%a%a%a%a%a%a%a%a%a%a"
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
  ; dyn_entry = false
  ; deprecated = None
  ; leading_comments = []
  }

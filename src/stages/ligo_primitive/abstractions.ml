type 'a t =
  { ty_binders : Var.Type_var.t list
  ; kind : Kind.t
  ; type_ : 'a
  }
[@@deriving eq, compare, yojson, hash, fold, map, iter, sexp]
(* Lambda (a : kind). term *)

let pp_forall f ppf { ty_binders; kind; type_ } : unit =
  Format.fprintf
    ppf
    "∀ (%a) : %a . %a"
    Simple_utils.PP_helpers.(list_sep_d Var.Type_var.pp)
    ty_binders
    Kind.pp
    kind
    f
    type_


let pp_type_abs f ppf { ty_binders; kind; type_ } : unit =
  Format.fprintf
    ppf
    "funtype (%a) : %a . %a"
    Simple_utils.PP_helpers.(list_sep_d Var.Type_var.pp)
    ty_binders
    Kind.pp
    kind
    f
    type_

module PP_helpers = Simple_utils.PP_helpers

type 'expr item =
  | Expr_entry of 'expr
  | Rest_entry of 'expr

and 'expr t = 'expr item list
[@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]

let pp f ppf (l : 'k item list) =
  let open Format in
  let open PP_helpers in
  let tuple_sep_expr value sep ppf t =
    let new_pp ppf v =
      match v with
      | Expr_entry v -> fprintf ppf "%a" value v
      | Rest_entry v -> fprintf ppf "...(%a)" value v
    in
    fprintf ppf "%a" (list_sep new_pp sep) t
  in
  fprintf ppf "@[<hv 2>[ %a ]@]" (tuple_sep_expr f (tag " ,@ ")) l

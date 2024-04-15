open Simple_utils

type 'a t = 'a List.Ne.t [@@deriving eq, compare, yojson, hash, fold, map]

let pp_list f ppf l =
  let open Format in
  let open Simple_utils.PP_helpers in
  let tuple_sep_expr value sep ppf t =
    let new_pp ppf v = fprintf ppf "%a" value v in
    fprintf ppf "%a" (list_sep new_pp sep) t
  in
  fprintf ppf "@[<hv 2>( %a )@]" (tuple_sep_expr f (tag " ,@ ")) l


let pp f ppf t = pp_list f ppf (List.Ne.to_list t)

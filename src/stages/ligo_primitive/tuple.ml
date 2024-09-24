module Ne_list = Simple_utils.Ne_list
module PP_helpers = Simple_utils.PP_helpers

type 'a t = 'a Ne_list.t [@@deriving eq, compare, yojson, hash, fold, map, bin_io]

let pp_list f ppf l =
  let open Format in
  let tuple_sep_expr value sep ppf t =
    let new_pp ppf v = fprintf ppf "%a" value v in
    fprintf ppf "%a" (PP_helpers.list_sep new_pp sep) t
  in
  fprintf ppf "@[<hv 2>( %a )@]" (tuple_sep_expr f (PP_helpers.tag " ,@ ")) l


let pp f ppf t = pp_list f ppf (Nonempty_list.to_list t)

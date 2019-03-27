open Format
module SMap = X_map.String

let const s ppf () = pp_print_string ppf s

let list_sep ?(pp_sep = const " ; ") pp =
  pp_print_list ~pp_sep pp


let pair_sep pp ppf (a, b) = fprintf ppf "(%a, %a)" pp a pp b
let smap_sep pp ppf m =
  let aux k v prev = (k, v) :: prev in
  let new_pp ppf (k, v) = fprintf ppf "%s -> %a" k pp v in
  let lst = List.rev @@ SMap.fold aux m [] in
  fprintf ppf "%a" (list_sep new_pp) lst

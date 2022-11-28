type 'a t = ('a * 'a) list [@@deriving eq, compare, yojson, hash, sexp, map, fold]

let pp' str f ppf m =
  let assoc ppf : 'a * 'a -> unit = fun (a, b) -> Format.fprintf ppf "%a -> %a" f a f b in
  Format.fprintf ppf "%s[%a]" str Simple_utils.PP_helpers.(list_sep_d assoc) m


let pp f ppf = pp' "map" f ppf
let pp_big_map f ppf = pp' "big_map" f ppf

let dedup_and_sort t ~compare =
  List.dedup_and_sort t ~compare:(fun (t11, t12) (t21, t22) ->
      let compare_1 = compare t11 t21 in
      if compare_1 = 0 then compare t12 t22 else compare_1)

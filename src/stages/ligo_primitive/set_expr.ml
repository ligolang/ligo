type 'a t = 'a list [@@deriving eq, compare, yojson, hash, sexp, map, fold]

let pp f ppf m = Format.fprintf ppf "set[%a]" Simple_utils.PP_helpers.(list_sep_d f) m
let dedup_and_sort t ~compare = List.dedup_and_sort t ~compare

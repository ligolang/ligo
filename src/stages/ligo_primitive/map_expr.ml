type 'a t = ('a * 'a) list
  [@@deriving eq, compare, yojson, hash, sexp]


let pp' str f ppf = fun m ->
  let assoc ppf : 'a * 'a -> unit =
    fun (a, b) -> Format.fprintf ppf "%a -> %a" f a f b
  in Format.fprintf ppf "%s[%a]" str
    Simple_utils.PP_helpers.(list_sep_d assoc) m

let pp f ppf = pp' "map" f ppf
let pp_big_map f ppf = pp' "big_map" f ppf


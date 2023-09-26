type t = { public : bool } [@@deriving eq, compare, yojson, hash]

open Format

let pp_if_set str ppf attr = if attr then fprintf ppf "[@@%s]" str else fprintf ppf ""
let pp ppf { public } = fprintf ppf "%a" (pp_if_set "private") (not public)
let default_attributes = { public = true }

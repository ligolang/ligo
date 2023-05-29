type t =
  { public : bool
  ; hidden : bool
  }
[@@deriving eq, compare, yojson, hash]

open Format

let pp_if_set str ppf attr = if attr then fprintf ppf "[@@%s]" str else fprintf ppf ""

let pp ppf { public; hidden } =
  fprintf ppf "%a%a" (pp_if_set "private") (not public) (pp_if_set "hidden") hidden


let default_attributes = { public = true; hidden = false }

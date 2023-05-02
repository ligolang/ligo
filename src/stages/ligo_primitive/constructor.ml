type 'e t =
  { constructor : Label.t
  ; element : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map, iter, sexp]

let pp f ppf { constructor; element } =
  Format.fprintf ppf "@[%a(%a)@]" Label.pp constructor f element

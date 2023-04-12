type 'a t =
  { lamb : 'a
  ; args : 'a
  }
[@@deriving eq, compare, yojson, hash, sexp, fold, iter, map]

let pp f ppf { lamb; args } = Format.fprintf ppf "@[<hv>(%a)@@(%a)@]" f lamb f args

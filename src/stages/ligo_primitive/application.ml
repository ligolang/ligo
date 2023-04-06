type 'a t =
  { lamb : 'a
  ; args : 'a
  }
[@@deriving eq, compare, yojson, hash, sexp, fold, iter, map]

let pp f ppf { lamb; args } = Format.fprintf ppf "@[<hv>(%a)@@(%a)@]" f lamb f args

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
 fun f acc { lamb; args } ->
  let acc, lamb = f acc lamb in
  let acc, args = f acc args in
  acc, { lamb; args }

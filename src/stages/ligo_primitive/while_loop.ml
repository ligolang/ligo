type 'e t =
  { cond : 'e
  ; body : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f ppf { cond; body } = Format.fprintf ppf "while %a do %a" f cond f body

let fold_map f acc { cond; body } =
  let acc, cond = f acc cond in
  let acc, body = f acc body in
  acc, { cond; body }

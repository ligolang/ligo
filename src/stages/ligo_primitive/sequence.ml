type 'e t =
  { expr1 : 'e
  ; expr2 : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map]

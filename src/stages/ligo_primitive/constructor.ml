type 'e t =
  { constructor : Label.t
  ; element : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f ppf { constructor; element } =
  Format.fprintf ppf "@[%a(%a)@]" Label.pp constructor f element


let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
 fun f acc { constructor; element } ->
  let acc, element = f acc element in
  acc, { constructor; element }

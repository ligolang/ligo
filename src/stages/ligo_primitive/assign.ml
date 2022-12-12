type ('e, 't) t =
  { binder : 't Binder.t
  ; expression : 'e
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f g ppf { binder; expression = e } =
  Format.fprintf ppf "%a := %a" (Binder.pp g) binder f e


let fold_map
    :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a, 'c) t
    -> 'acc * ('b, 'd) t
  =
 fun f g acc { binder; expression } ->
  let acc, binder = Binder.fold_map g acc binder in
  let acc, expression = f acc expression in
  acc, { binder; expression }

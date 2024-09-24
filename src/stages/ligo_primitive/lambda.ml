type ('e, 't) t =
  { binder : 't Param.t
  ; output_type : 't
  ; result : 'e
  }
[@@deriving eq, compare, yojson, hash, iter, fold, sexp, map, bin_io]

let pp f g ppf { binder; output_type; result } =
  Format.fprintf ppf "fun (%a)%a -> %a" (Param.pp g) binder g output_type f result


let fold_map
    :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a, 'c) t
    -> 'acc * ('b, 'd) t
  =
 fun f g acc { binder; output_type; result } ->
  let acc, binder = Param.fold_map g acc binder in
  let acc, output_type = g acc output_type in
  let acc, result = f acc result in
  acc, { binder; output_type; result }

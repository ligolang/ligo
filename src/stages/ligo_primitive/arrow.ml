type 'a t = {
  type1: 'a ;
  type2: 'a ;
  } [@@deriving eq,compare,yojson,hash,fold,map, sexp]

let pp g ppf = fun {type1;type2} ->
  Format.fprintf ppf "%a -> %a"
    g type1
    g type2

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun g acc {type1;type2} ->
  let acc,type1 = g acc type1 in
  let acc,type2 = g acc type2 in
  (acc,{type1;type2})

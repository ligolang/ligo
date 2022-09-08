type 'e t = {
  expr1: 'e ;
  expr2: 'e ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f ppf = fun {expr1;expr2} ->
  Format.fprintf ppf "{@[<v 2>@,%a;@,%a@]@,}"
    f expr1
    f expr2

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {expr1;expr2} ->
  let acc,expr1 = f acc expr1 in
  let acc,expr2 = f acc expr2 in
  (acc,{expr1;expr2})

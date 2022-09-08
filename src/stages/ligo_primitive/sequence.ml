type 'e t = {
  expr1: 'e ;
  expr2: 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {expr1;expr2} ->
  Format.fprintf ppf "{@[<v 2>@,%a;@,%a@]@,}"
    f expr1
    f expr2

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {expr1;expr2} ->
  let acc = f acc expr1 in
  let acc = f acc expr2 in
  acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f {expr1;expr2} ->
  let expr1 = f expr1 in
  let expr2 = f expr2 in
  {expr1;expr2}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {expr1;expr2} ->
  let acc,expr1 = f acc expr1 in
  let acc,expr2 = f acc expr2 in
  (acc,{expr1;expr2})

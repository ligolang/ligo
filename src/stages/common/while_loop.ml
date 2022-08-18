type 'e t = {
  cond : 'e ;
  body : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {cond; body} ->
  Format.fprintf ppf "while %a do %a"
    f cond
    f body

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {cond; body} ->
  let acc = f acc cond in
  let acc = f acc body in
  acc

let map
= fun f {cond; body} ->
  let cond = f cond in
  let body = f body in
  {cond; body}

let fold_map
= fun f acc {cond; body} ->
  let acc,cond = f acc cond in
  let acc,body = f acc body in
  (acc, {cond; body})

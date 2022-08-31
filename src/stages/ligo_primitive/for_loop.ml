type 'e t = {
  binder : Var.ValueVar.t ;
  start  : 'e ;
  final  : 'e ;
  incr   : 'e ;
  f_body : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {binder; start; final; incr; f_body} ->
  Format.fprintf ppf "for %a from %a to %a by %a do %a"
    Var.ValueVar.pp binder
    f start
    f final
    f incr
    f f_body

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {binder=_;start;final;incr;f_body} ->
  let acc = f acc start in
  let acc = f acc final in
  let acc = f acc incr in
  let acc = f acc f_body in
  acc

let map
= fun f {binder; start; final; incr; f_body} ->
  let f_body = f f_body in
  {binder; start; final; incr; f_body}

let fold_map
= fun f acc {binder; start; final; incr; f_body} ->
  let acc,f_body = f acc f_body in
  (acc, {binder; start; final; incr; f_body})

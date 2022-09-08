type ('e,'t) t = {
  binder      : 't Binder.t ;
  expression  : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f g ppf = fun {binder; expression=e} ->
  Format.fprintf ppf "%a := %a"
    (Binder.pp g) binder
    f e

let fold : ('acc -> 'a -> 'acc) -> ('acc -> 'b -> 'acc) -> 'acc -> ('a,'b) t -> 'acc
= fun f g acc {binder; expression} ->
  let acc = Binder.fold g acc binder in
  let acc = f acc expression in
  acc

let map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) t -> ('b,'d) t
= fun f g {binder; expression} ->
  let binder      = Binder.map g binder in
  let expression  = f expression in
  {binder; expression}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {binder; expression} ->
  let acc,binder      = Binder.fold_map g acc binder in
  let acc,expression  = f acc expression in
  (acc, {binder; expression})

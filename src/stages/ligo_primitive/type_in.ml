type ('e, 't) t = {
    type_binder: Var.TypeVar.t ;
    rhs        : 't ;
    let_result : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f g ppf = fun {type_binder; rhs; let_result} ->
  Format.fprintf ppf "@[let %a =@;<1 2>%a in@ %a@]"
    Var.TypeVar.pp type_binder
    g rhs
    f let_result

let fold : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) t -> 'acc
= fun f g acc { type_binder=_; rhs ; let_result} ->
  let acc = g acc rhs in
  let acc = f acc let_result in
  acc

let map :  ('a -> 'b) -> ('c -> 'd) -> ('a,'c) t -> ('b,'d) t
= fun f g {type_binder; rhs; let_result} ->
  let rhs        = g rhs in
  let let_result = f let_result in
  {type_binder; rhs; let_result}

let fold_map :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {type_binder; rhs; let_result} ->
  let acc,rhs        = g acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{type_binder; rhs; let_result})

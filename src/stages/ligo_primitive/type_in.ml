type ('e, 't) t = {
    type_binder: Var.TypeVar.t ;
    rhs        : 't ;
    let_result : 'e ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f g ppf = fun {type_binder; rhs; let_result} ->
  Format.fprintf ppf "@[let %a =@;<1 2>%a in@ %a@]"
    Var.TypeVar.pp type_binder
    g rhs
    f let_result

let fold_map :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {type_binder; rhs; let_result} ->
  let acc,rhs        = g acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{type_binder; rhs; let_result})

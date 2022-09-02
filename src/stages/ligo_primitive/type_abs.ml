type 'e t = {
  type_binder : Var.TypeVar.t;
  result : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let pp f ppf = fun {type_binder;result} ->
  Format.fprintf ppf "Λ %a -> @;@[%a@]"
    Var.TypeVar.pp type_binder
    f result

let fold : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc
= fun f acc {type_binder=_;result}->
  let acc = f acc result in
  acc

let map : ('a -> 'b) -> 'a t -> 'b t
= fun f {type_binder;result}->
  let result = f result in
  {type_binder;result}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {type_binder;result}->
  let acc,result = f acc result in
  acc,{type_binder;result}

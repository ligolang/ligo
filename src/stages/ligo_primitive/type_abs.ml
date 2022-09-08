type 'e t = {
  type_binder : Var.Type_var.t;
  result : 'e ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f ppf = fun {type_binder;result} ->
  Format.fprintf ppf "Λ %a -> @;@[%a@]"
    Var.Type_var.pp type_binder
    f result

let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
= fun f acc {type_binder;result}->
  let acc,result = f acc result in
  acc,{type_binder;result}

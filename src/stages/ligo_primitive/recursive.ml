type ('e, 't) t = {
  fun_name :  Var.ValueVar.t ;
  fun_type : 't ;
  lambda   : ('e, 't) Lambda.t ;
  } [@@deriving eq,compare,yojson,hash]

let pp f g ppf = fun { fun_name;fun_type; lambda=l} ->
  Format.fprintf ppf "rec (%a%a => %a)"
    Var.ValueVar.pp fun_name
    g fun_type
    (Lambda.pp f g) l

let fold : ('acc -> 'a -> 'acc) -> ('acc -> 'c -> 'acc) -> 'acc -> ('a,'c) t -> 'acc
= fun f g acc {fun_name=_;fun_type;lambda=l} ->
  let acc = g acc fun_type in
  let acc = Lambda.fold f g acc l in
   acc

let map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) t -> ('b,'d) t
= fun f g {fun_name;fun_type;lambda=l} ->
  let fun_type = g fun_type in
  let lambda = Lambda.map f g l in
  {fun_name;fun_type;lambda}

let fold_map : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {fun_name;fun_type;lambda} ->
  let acc,fun_type = g acc fun_type in
  let acc,lambda = Lambda.fold_map f g acc lambda in
  (acc,{fun_name;fun_type;lambda})

type ('e, 't) t = {
  fun_name :  Var.Value_var.t ;
  fun_type : 't ;
  lambda   : ('e, 't) Lambda.t ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f g ppf = fun { fun_name;fun_type; lambda=l} ->
  Format.fprintf ppf "rec (%a%a => %a)"
    Var.Value_var.pp fun_name
    g fun_type
    (Lambda.pp f g) l

let fold_map : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
= fun f g acc {fun_name;fun_type;lambda} ->
  let acc,fun_type = g acc fun_type in
  let acc,lambda = Lambda.fold_map f g acc lambda in
  (acc,{fun_name;fun_type;lambda})

type 'e t = {
  binder : Var.Value_var.t ;
  start  : 'e ;
  final  : 'e ;
  incr   : 'e ;
  f_body : 'e ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let pp f ppf = fun {binder; start; final; incr; f_body} ->
  Format.fprintf ppf "for %a from %a to %a by %a do %a"
    Var.Value_var.pp binder
    f start
    f final
    f incr
    f f_body

(* WTF, fold_map is wrong? *)
let fold_map
= fun f acc {binder; start; final; incr; f_body} ->
  let acc,f_body = f acc f_body in
  (acc, {binder; start; final; incr; f_body})

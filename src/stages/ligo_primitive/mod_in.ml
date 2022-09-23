  type ('e,'dcl) t = {
    module_binder : Var.Module_var.t ;
    rhs           : 'dcl;
    let_result    : 'e ;
  } [@@deriving eq,compare,yojson,hash,fold,map]

let fold_map :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc ->
  ('a,'c) t -> 'acc * ('b,'d) t
= fun f h acc {module_binder; rhs; let_result} ->
  let acc,rhs        = h acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{module_binder; rhs; let_result})

let pp f h ppf = fun {module_binder; rhs; let_result;} ->
  Format.fprintf ppf "@[module %a =@;<1 2>%a in@ %a@]"
    Var.Module_var.pp module_binder
    h rhs
    f let_result

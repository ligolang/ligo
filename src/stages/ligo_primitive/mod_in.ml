  type ('e,'dcl) t = {
    module_binder : Var.ModuleVar.t ;
    rhs           : 'dcl;
    let_result    : 'e ;
  } [@@deriving eq,compare,yojson,hash]

let fold : ('acc -> 'exp -> 'acc) -> ('acc -> 'ty_exp -> 'acc) -> 'acc -> ('exp,'ty_exp) t -> 'acc
= fun f g acc { module_binder=_; rhs ; let_result} ->
  let acc = g acc rhs in
  let acc = f acc let_result in
  acc

let map : ('e_src -> 'e_dst) -> ('dcl_src -> 'dcl_dst) ->
  ('e_src,'dcl_src) t -> ('e_dst,'dcl_dst) t
= fun map_e map_dcl { module_binder; rhs ; let_result} ->
  let rhs = map_dcl rhs in
  let let_result = map_e let_result in
  { module_binder; rhs ; let_result}

let fold_map :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc ->
  ('a,'c) t -> 'acc * ('b,'d) t
= fun f h acc {module_binder; rhs; let_result} ->
  let acc,rhs        = h acc rhs in
  let acc,let_result = f acc let_result in
  (acc,{module_binder; rhs; let_result})

let pp f h ppf = fun {module_binder; rhs; let_result;} ->
  Format.fprintf ppf "@[module %a =@;<1 2>%a in@ %a@]"
    Var.ModuleVar.pp module_binder
    h rhs
    f let_result

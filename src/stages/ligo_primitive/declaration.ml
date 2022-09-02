module Option = Simple_utils.Option
module Location = Simple_utils.Location
module type Attr = sig
  type t
    [@@deriving eq,compare,yojson,hash]
  val  pp : Format.formatter -> t -> unit
end

module ValueDecl (Attr : Attr) = struct
  type ('e,'t) t = {
      binder : 't Binder.t;
      expr : 'e ;
      attr : Attr.t;
    } [@@deriving eq,compare,yojson,hash]

  let fold : ('acc -> 'exp -> 'acc) -> ('acc -> 'ty_exp -> 'acc) -> 'acc -> ('exp,'ty_exp) t -> 'acc
  = fun f g acc {binder; attr=_; expr} ->
    let acc = Binder.fold g acc binder in
    let acc = f acc expr     in
    acc

  let map : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) t -> ('b,'d) t
  = fun map_e map_ty {binder; attr; expr} ->
    let binder = Binder.map map_ty binder in
    let expr   = map_e expr     in
    {binder;attr;expr}

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) t -> 'acc * ('b,'d) t
  = fun f g acc {binder; attr; expr} ->
    let acc,binder = Binder.fold_map g acc binder in
    let acc,expr   = f acc expr     in
    (acc,{binder;attr;expr})

  let pp ?(print_type = true) f g ppf = fun {binder; attr ; expr} ->
    let cond ppf b =
      if print_type then
        Format.fprintf ppf "%a" (Binder.pp g) b
      else
        Format.fprintf ppf "%a" Var.ValueVar.pp b.var
    in
    Format.fprintf ppf "@[<2>const %a =@ %a%a@]"
      cond binder
      f expr
      Attr.pp attr

end

module TypeDecl (Attr:Attr) = struct
  type 't t = {
      type_binder : Var.TypeVar.t ;
      type_expr : 't ;
      type_attr : Attr.t ;
    } [@@deriving eq,compare,yojson,hash]

  let fold : ('acc -> 'a -> 'acc) -> 'acc -> ('a) t -> 'acc
  = fun g acc {type_binder=_; type_expr; type_attr=_} ->
    let acc = g acc type_expr in
    acc

  let map : ('a -> 'b) -> ('a) t -> ('b) t
  = fun g {type_binder; type_expr; type_attr} ->
    let type_expr = g type_expr in
    {type_binder; type_expr; type_attr}

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t
  = fun g acc {type_binder; type_expr; type_attr} ->
    let acc,type_expr = g acc type_expr in
    (acc,{type_binder; type_expr; type_attr})

  let pp g ppf = fun {type_binder;type_expr; type_attr} ->
    Format.fprintf ppf "@[<2>type %a =@ %a%a@]"
      Var.TypeVar.pp type_binder
      g type_expr
      Attr.pp type_attr

end

module ModuleDecl (Attr : Attr) = struct
  type ('module_expr) t = {
      module_binder : Var.ModuleVar.t ;
      module_ : 'module_expr;
      module_attr : Attr.t
    } [@@deriving eq,compare,yojson,hash]

  let fold : ('acc -> 'dcl -> 'acc) -> 'acc -> ('dcl) t -> 'acc
  = fun f acc {module_binder=_;module_;module_attr=_} ->
    let acc = f acc module_ in
    acc

  let map : ('dcl_src -> 'dcl_dst) -> ('dcl_src) t -> ('dcl_dst) t
  = fun map {module_binder; module_; module_attr} ->
    let module_ = map module_ in
    {module_binder;module_;module_attr}

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc ->
    ('a) t -> 'acc * ('b) t
  = fun f acc {module_binder; module_;module_attr} ->
    let acc,module_ = f acc module_ in
    (acc, {module_binder;module_;module_attr})

  let pp h ppf = fun {module_binder;module_;module_attr} ->
    Format.fprintf ppf "@[<2>module %a =@ %a%a@]"
      Var.ModuleVar.pp module_binder
      h module_
      Attr.pp module_attr

end

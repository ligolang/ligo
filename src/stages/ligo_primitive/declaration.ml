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
    } [@@deriving eq,compare,yojson,hash,fold,map]

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
    } [@@deriving eq,compare,yojson,hash,fold,map]

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
    } [@@deriving eq,compare,yojson,hash,fold,map]

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

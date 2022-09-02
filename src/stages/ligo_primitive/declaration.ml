module Option = Simple_utils.Option
module Location = Simple_utils.Location
module type Attr = sig
  type value
    [@@deriving eq,compare,yojson,hash]
  type type_
    [@@deriving eq,compare,yojson,hash]
  type module_
    [@@deriving eq,compare,yojson,hash]
  val  pp_value  : Format.formatter -> value   -> unit
  val  pp_type   : Format.formatter -> type_   -> unit
  val  pp_module : Format.formatter -> module_ -> unit
end

module Make (Attr : Attr) = struct
  (*** Declarations language ***)
  type ('e,'t) declaration_constant = {
      binder : 't Binder.t;
      expr : 'e ;
      attr : Attr.value;
    } [@@deriving eq,compare,yojson,hash]


  type 't declaration_type = {
      type_binder : Var.TypeVar.t ;
      type_expr : 't ;
      type_attr : Attr.type_ ;
    } [@@deriving eq,compare,yojson,hash]

  type ('dcl) declaration_module = {
      module_binder : Var.ModuleVar.t ;
      module_ : 'dcl Module_expr.t Location.wrap;
      module_attr : Attr.module_
    } [@@deriving eq,compare,yojson,hash]

  and ('e,'t,'dcl) declaration =
    | Declaration_type     of ('t) declaration_type
    | Declaration_constant of ('e,'t option) declaration_constant
    | Declaration_module   of ('dcl) declaration_module

  (*** Modules language ***)

  module Fold = struct
    let rec declaration_constant : ('acc -> 'exp -> 'acc) -> ('acc -> 'ty_exp -> 'acc) -> 'acc -> ('exp,'ty_exp option) declaration_constant -> 'acc
    = fun f g acc {binder; attr=_; expr} ->
      let acc = Binder.fold (fun init -> Option.fold ~f:g ~init) acc binder in
      let acc = f acc expr     in
      acc

    and declaration_type : ('acc -> 'a -> 'acc) -> 'acc -> ('a) declaration_type -> 'acc
    = fun g acc {type_binder=_; type_expr; type_attr=_} ->
      let acc = g acc type_expr in
      acc

    and declaration_module : ('acc -> 'dcl -> 'acc) -> 'acc -> ('dcl) declaration_module -> 'acc
    = fun f acc {module_binder=_;module_;module_attr=_} ->
      let acc = Location.fold (Module_expr.fold f) acc module_ in
      acc

    and declaration : ('acc -> 'exp -> 'acc) -> ('acc -> 'ty_exp -> 'acc) -> ('acc -> 'dcl -> 'acc) -> 'acc -> ('exp,'ty_exp,'dcl) declaration -> 'acc
    = fun f g h acc d ->
      match d with
      | Declaration_type    ty -> declaration_type       g acc ty
      | Declaration_constant c -> declaration_constant f g acc c
      | Declaration_module   m -> declaration_module   h acc m

  end
  module Map = struct
    let declaration_constant : ('a -> 'b) -> ('c -> 'd) -> ('a,'c) declaration_constant -> ('b,'d) declaration_constant
    = fun map_e map_ty {binder; attr; expr} ->
      let binder = Binder.map map_ty binder in
      let expr   = map_e expr     in
      {binder;attr;expr}

    let declaration_type : ('a -> 'b) -> ('a) declaration_type -> ('b) declaration_type
    = fun g {type_binder; type_expr; type_attr} ->
      let type_expr = g type_expr in
      {type_binder; type_expr; type_attr}

    let rec declaration_module : ('dcl_src -> 'dcl_dst) ->
      ('dcl_src) declaration_module -> ('dcl_dst) declaration_module
    = fun map {module_binder; module_; module_attr} ->
      let module_ = Location.map (Module_expr.map map) module_ in
      {module_binder;module_;module_attr}

    and module_alias
    = fun ma -> ma

    and declarations f g prg =
    List.map ~f:(declaration f g) prg

    and declaration
    = fun map_e map_ty map_dcl -> function
      Declaration_type    ty -> let ty = declaration_type           map_ty ty in Declaration_type ty
    | Declaration_constant c -> let c  = declaration_constant map_e (Option.map ~f:map_ty) c  in Declaration_constant c
    | Declaration_module   m -> let m  = declaration_module        map_dcl m  in Declaration_module m

  end
  module Fold_map = struct
    let declaration_constant : ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a,'c) declaration_constant -> 'acc * ('b,'d) declaration_constant
    = fun f g acc {binder; attr; expr} ->
      let acc,binder = Binder.fold_map g acc binder in
      let acc,expr   = f acc expr     in
      (acc,{binder;attr;expr})

    let declaration_type : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a declaration_type -> 'acc * 'b declaration_type
    = fun g acc {type_binder; type_expr; type_attr} ->
      let acc,type_expr = g acc type_expr in
      (acc,{type_binder; type_expr; type_attr})

    let rec declaration_module : ('acc -> 'a -> 'acc * 'b) -> 'acc ->
      ('a) declaration_module -> 'acc * ('b) declaration_module
    = fun f acc {module_binder; module_;module_attr} ->
      let acc,module_ = Location.fold_map (Module_expr.fold_map f) acc module_ in
      (acc, {module_binder;module_;module_attr})

    and module_alias = fun acc ma ->  (acc, ma)

    and declaration :  ('acc -> 'a -> 'acc * 'b) -> (_) -> (_) -> 'acc -> ('a,'c,'e) declaration -> 'acc * ('b,'d,'f) declaration
    = fun f g h acc d ->
      match d with
      | Declaration_type    ty -> let (acc,ty) = declaration_type      g acc ty in (acc, (Declaration_type     ty))
      | Declaration_constant c -> let (acc,c)  = declaration_constant f (Option.fold_map g) acc c in (acc, (Declaration_constant c))
      | Declaration_module   m -> let (acc,m)  = declaration_module     h acc m in (acc, (Declaration_module   m))

  end
  module PP = struct
    let rec declaration_constant ?(print_type = true) f g ppf = fun {binder; attr ; expr} ->
      let cond ppf b =
        if print_type then
          Format.fprintf ppf "%a" (Binder.pp g) b
        else
          Format.fprintf ppf "%a" Var.ValueVar.pp b.var
      in
      Format.fprintf ppf "@[<2>const %a =@ %a%a@]"
        cond binder
        f expr
        Attr.pp_value attr

    and declaration_type g ppf = fun {type_binder;type_expr; type_attr} ->
      Format.fprintf ppf "@[<2>type %a =@ %a%a@]"
        Var.TypeVar.pp type_binder
        g type_expr
        Attr.pp_type type_attr

    and declaration_module h ppf = fun {module_binder;module_;module_attr} ->
      Format.fprintf ppf "@[<2>module %a =@ %a%a@]"
        Var.ModuleVar.pp module_binder
        (Location.pp_wrap (Module_expr.pp h)) module_
        Attr.pp_module module_attr

    and declaration ?(print_type = true) f g h ppf = function
      | Declaration_type    ty -> declaration_type g ppf ty
      | Declaration_constant c -> declaration_constant ~print_type f (Format.pp_print_option g) ppf c
      | Declaration_module   m -> declaration_module h ppf m

  end
end

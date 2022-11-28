module Option = Simple_utils.Option
module Location = Simple_utils.Location

module type Attr = sig
  type t [@@deriving eq, compare, yojson, hash]

  val pp : Format.formatter -> t -> unit
  val default_attributes : t
end

module Value_decl (Attr : Attr) = struct
  type ('e, 't) t =
    { binder : 't Binder.t
    ; expr : 'e
    ; attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let fold_map
      :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a, 'c) t
      -> 'acc * ('b, 'd) t
    =
   fun f g acc { binder; attr; expr } ->
    let acc, binder = Binder.fold_map g acc binder in
    let acc, expr = f acc expr in
    acc, { binder; attr; expr }


  let pp ?(print_type = true) f g ppf { binder; attr; expr } =
    let cond ppf b =
      if print_type
      then Format.fprintf ppf "%a" (Binder.pp g) b
      else Format.fprintf ppf "%a" (Binder.pp (fun _ _ -> ())) b
    in
    Format.fprintf ppf "@[<2>const %a =@ %a%a@]" cond binder f expr Attr.pp attr
end

module Pattern_decl (Pattern : Pattern.S) (Attr : Attr) = struct
  type ('e, 't) t =
    { pattern : 't Pattern.t
    ; expr : 'e
    ; attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let fold_map
      :  ('acc -> 'a -> 'acc * 'b) -> ('acc -> 'c -> 'acc * 'd) -> 'acc -> ('a, 'c) t
      -> 'acc * ('b, 'd) t
    =
   fun f g acc { pattern; attr; expr } ->
    let acc, pattern = Pattern.fold_map g acc pattern in
    let acc, expr = f acc expr in
    acc, { pattern; attr; expr }


  let pp ?(print_type = true) f g ppf { pattern; attr; expr } =
    let cond ppf b =
      if print_type
      then Format.fprintf ppf "%a" (Pattern.pp g) b
      else Format.fprintf ppf "%a" (Pattern.pp (fun _ _ -> ())) b
    in
    Format.fprintf ppf "@[<2>let (%a) =@ %a%a@]" cond pattern f expr Attr.pp attr
end

module Type_decl (Attr : Attr) = struct
  type 't t =
    { type_binder : Var.Type_var.t
    ; type_expr : 't
    ; type_attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
   fun g acc { type_binder; type_expr; type_attr } ->
    let acc, type_expr = g acc type_expr in
    acc, { type_binder; type_expr; type_attr }


  let pp g ppf { type_binder; type_expr; type_attr } =
    Format.fprintf
      ppf
      "@[<2>type %a =@ %a%a@]"
      Var.Type_var.pp
      type_binder
      g
      type_expr
      Attr.pp
      type_attr
end

module Module_decl (Attr : Attr) = struct
  type 'module_expr t =
    { module_binder : Var.Module_var.t
    ; module_ : 'module_expr
    ; module_attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let fold_map : ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a t -> 'acc * 'b t =
   fun f acc { module_binder; module_; module_attr } ->
    let acc, module_ = f acc module_ in
    acc, { module_binder; module_; module_attr }


  let pp h ppf { module_binder; module_; module_attr } =
    Format.fprintf
      ppf
      "@[<2>module %a =@ %a%a@]"
      Var.Module_var.pp
      module_binder
      h
      module_
      Attr.pp
      module_attr
end

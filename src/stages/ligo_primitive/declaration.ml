open Var
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
  type ('module_expr, 'annotation) t =
    { module_binder : Var.Module_var.t
    ; module_ : 'module_expr
    ; annotation : 'annotation
    ; module_attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp h a ppf { module_binder; module_; annotation; module_attr } =
    Format.fprintf
      ppf
      "@[<2>module %a : %a =@ %a%a@]"
      Var.Module_var.pp
      module_binder
      a
      annotation
      h
      module_
      Attr.pp
      module_attr
end

module Signature_decl (Attr : Attr) = struct
  type 'signature_expr t =
    { signature_binder : Var.Module_var.t
    ; signature : 'signature_expr
    ; signature_attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp h ppf { signature_binder; signature; signature_attr } =
    Format.fprintf
      ppf
      "@[<2>module type %a =@ %a%a@]"
      Var.Module_var.pp
      signature_binder
      h
      signature
      Attr.pp
      signature_attr
end

module Import_decl (Attr : Attr) = struct
  type t =
    { import_name : Module_var.t
    ; imported_module : Module_var.t
    ; import_attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp ppf { import_name; imported_module; import_attr } =
    Format.fprintf
      ppf
      "@[<2>import %a =@ %a%a@]"
      Module_var.pp
      import_name
      Module_var.pp
      imported_module
      Attr.pp
      import_attr
end

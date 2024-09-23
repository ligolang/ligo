open Var
module Location = Simple_utils.Location

module type Attr = sig
  type t [@@deriving eq, compare, yojson, hash, bin_io]

  val pp : Format.formatter -> t -> unit
  val default_attributes : t
end

module Value_decl (Attr : Attr) = struct
  type ('e, 't) t =
    { binder : 't Binder.t
    ; expr : 'e
    ; attr : Attr.t
    }
  [@@deriving eq, compare, yojson, hash, fold, map, bin_io]

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
  [@@deriving eq, compare, yojson, hash, fold, map, bin_io]

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
  [@@deriving eq, compare, yojson, hash, fold, map, bin_io]

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
  [@@deriving eq, compare, yojson, hash, fold, map, bin_io]

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
  module Ne_list = Simple_utils.Ne_list

  type t =
    | Import_rename of
        { alias : Module_var.t
        ; imported_module : Module_var.t
        ; import_attr : Attr.t
        }
    | Import_all_as of
        { alias : Module_var.t
        ; module_str : string
        ; import_attr : Attr.t
        }
    | Import_selected of
        { imported : Value_var.t Ne_list.t
        ; module_str : string
        ; import_attr : Attr.t
        }
  [@@deriving eq, compare, yojson, hash, fold, map]

  let pp ppf = function
    | Import_rename { alias; imported_module; import_attr } ->
      Format.fprintf
        ppf
        "@[<2>import %a =@ %a%a@]"
        Module_var.pp
        alias
        Module_var.pp
        imported_module
        Attr.pp
        import_attr
    | Import_all_as { alias; module_str; import_attr } ->
      Format.fprintf
        ppf
        "@[<2>import * as %a from %s%a@]"
        Module_var.pp
        alias
        module_str
        Attr.pp
        import_attr
    | Import_selected { imported; module_str; import_attr } ->
      let imported : Value_var.t list =
        match imported with
        | x :: l -> x :: l
      in
      let rec pp_imported ppf = function
        | [] -> ()
        | [ x ] -> Format.fprintf ppf "%a" Value_var.pp x
        | x :: l ->
          Format.fprintf ppf "%a, " Value_var.pp x;
          pp_imported ppf l
      in
      Format.fprintf
        ppf
        "@[<2>import { %a } from %s%a@]"
        pp_imported
        imported
        module_str
        Attr.pp
        import_attr
end

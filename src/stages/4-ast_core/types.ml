open Ligo_prim
module Location = Simple_utils.Location
module Row = Row.With_optional_layout
module Ne_list = Simple_utils.Ne_list

type type_content =
  | T_variable of Type_var.t
  | T_constant of Literal_types.t * int
  | T_contract_parameter of Module_var.t Ne_list.t
  | T_sum of row
  | T_union of type_expression Union.t
  | T_record of row
  | T_arrow of type_expression Arrow.t
  | T_app of (Type_var.t Module_access.t, type_expression) Type_app.t
  | T_module_accessor of Type_var.t Module_access.t
  | T_singleton of Literal_value.t
  | T_abstraction of type_expression Abstraction.t
  | T_for_all of type_expression Abstraction.t

and row = type_expression Row.t

and type_expression =
  { type_content : type_content
  ; location : Location.t [@deriving.ignore] [@hash.ignore]
  }
[@@deriving eq, compare, yojson, hash, bin_io]
and type_expression_option = type_expression option [@@deriving eq, compare, yojson, hash]

type ty_expr = type_expression [@@deriving eq, compare, yojson, hash, bin_io]

module ValueAttr = Value_attr
module TypeOrModuleAttr = Type_or_module_attr
module SigItemAttr = Sig_item_attr
module SigTypeAttr = Sig_type_attr
module Accessor = Accessor (Access_label)
module Update = Update (Access_label)
module Value_decl = Value_decl (Value_attr)
module Type_decl = Type_decl (Type_or_module_attr)
module Module_decl = Module_decl (Type_or_module_attr)
module Signature_decl = Signature_decl (Signature_attr)
module Pattern = Linear_pattern
module Match_expr = Match_expr.Make (Pattern)
module Pattern_decl = Pattern_decl (Pattern) (Value_attr)
module Let_in = Let_in.Make (Pattern) (Value_attr)
module Import_decl = Import_decl (Type_or_module_attr)

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_contract of Module_var.t Ne_list.t
  | E_literal of Literal_value.t
  | E_constant of
      expression Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expression Application.t
  | E_lambda of (expression, ty_expr option) Lambda.t
  | E_recursive of (expression, ty_expr) Recursive.t
  | E_type_abstraction of expression Type_abs.t
  | E_let_in of (expression, ty_expr option) Let_in.t
  | E_type_in of (expression, ty_expr) Type_in.t
  | E_mod_in of (expression, module_expr) Mod_in.t
  | E_raw_code of expression Raw_code.t
  (* Variant *)
  | E_constructor of expression Constructor.t (* For user defined constructors *)
  | E_matching of (expression, ty_expr option) Match_expr.t
  (* Record *)
  | E_record of expression Record.t
  | E_tuple of expression Tuple.t
  | E_array of expression Array_repr.t
  | E_array_as_list of expression Array_repr.t
  | E_accessor of expression Accessor.t
  | E_update of expression Update.t
  (* Advanced *)
  | E_ascription of (expression, ty_expr) Ascription.t
  | E_module_accessor of Value_var.t Module_access.t
  (* Imperative *)
  | E_let_mut_in of (expression, ty_expr option) Let_in.t
  | E_assign of (expression, ty_expr option) Assign.t
  | E_for of expression For_loop.t
  | E_for_each of expression For_each_loop.t
  | E_while of expression While_loop.t
[@@deriving eq, compare, yojson, hash]

and expression =
  { expression_content : expression_content
  ; location : Location.t [@hash.ignore]
  }
[@@deriving bin_io]

and module_annotation =
  { signature : signature_expr
  ; filter : bool (** [false] for JsLIGO, [true] for CameLIGO *)
  }
[@@deriving yojson, eq, compare, hash]

and declaration_content =
  | D_value of (expression, ty_expr option) Value_decl.t
  | D_irrefutable_match of (expression, ty_expr option) Pattern_decl.t
  | D_type of ty_expr Type_decl.t
  | D_module of (module_expr, module_annotation option) Module_decl.t
  | D_module_include of module_expr
  | D_signature of signature_expr Signature_decl.t
  | D_import of Import_decl.t

and declaration = declaration_content Location.wrap [@@deriving bin_io]
and module_expr_content = declaration Module_expr.t
and module_expr = module_expr_content Location.wrap [@@deriving eq, compare, yojson, hash]

and sig_item_content =
  | S_value of Value_var.t * ty_expr * Sig_item_attr.t
  | S_type of Type_var.t * ty_expr * Sig_type_attr.t
  | S_type_var of Type_var.t * Sig_type_attr.t
  | S_module of Module_var.t * signature
  | S_module_type of Module_var.t * signature
  | S_include of signature_expr

and sig_item = sig_item_content Location.wrap
and signature = { items : sig_item list }

and signature_content =
  | S_sig of signature
  | S_path of Module_var.t Ne_list.t

and signature_expr = signature_content Location.wrap

type expr = expression [@@deriving eq, compare, yojson, hash, bin_io]
type decl = declaration [@@deriving eq, compare, yojson, hash, bin_io]

type module_ = decl list [@@deriving eq, compare, yojson, hash]
type program = declaration list [@@deriving eq, compare, yojson, hash]

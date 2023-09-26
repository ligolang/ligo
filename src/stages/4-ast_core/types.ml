open Ligo_prim
module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string
module Row = Row.With_optional_layout

type type_content =
  | T_variable of Type_var.t
  | T_constant of Literal_types.t * int
  | T_contract_parameter of Module_var.t List.Ne.t
  | T_sum of row
  | T_record of row
  | T_arrow of ty_expr Arrow.t
  | T_app of (Type_var.t Module_access.t, ty_expr) Type_app.t
  | T_module_accessor of Type_var.t Module_access.t
  | T_singleton of Literal_value.t
  | T_abstraction of ty_expr Abstraction.t
  | T_for_all of ty_expr Abstraction.t

and row = type_expression Row.t

and type_expression =
  { type_content : type_content
  ; location : Location.t [@deriving.ignore] [@hash.ignore]
  }

and ty_expr = type_expression [@@deriving eq, compare, yojson, hash]
and type_expression_option = type_expression option [@@deriving eq, compare, yojson, hash]

module ValueAttr = Value_attr
module TypeOrModuleAttr = Type_or_module_attr
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

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_contract of Module_var.t Simple_utils.List.Ne.t
  | E_literal of Literal_value.t
  | E_constant of
      expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr option) Lambda.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_type_abstraction of expr Type_abs.t
  | E_let_in of (expr, ty_expr option) Let_in.t
  | E_type_in of (expr, ty_expr) Type_in.t
  | E_mod_in of (expr, module_expr) Mod_in.t
  | E_raw_code of expr Raw_code.t
  (* Variant *)
  | E_constructor of expr Constructor.t (* For user defined constructors *)
  | E_matching of (expr, ty_expr option) Match_expr.t
  (* Record *)
  | E_record of expr Record.t
  | E_accessor of expr Accessor.t
  | E_update of expr Update.t
  (* Advanced *)
  | E_ascription of (expr, ty_expr) Ascription.t
  | E_module_accessor of Value_var.t Module_access.t
  (* Imperative *)
  | E_let_mut_in of (expr, ty_expr option) Let_in.t
  | E_assign of (expr, ty_expr option) Assign.t
  | E_for of expr For_loop.t
  | E_for_each of expr For_each_loop.t
  | E_while of expr While_loop.t

and expression =
  { expression_content : expression_content
  ; location : Location.t [@hash.ignore]
  }

and expr = expression [@@deriving eq, compare, yojson, hash]

and declaration_content =
  | D_value of (expr, ty_expr option) Value_decl.t
  | D_irrefutable_match of (expr, ty_expr option) Pattern_decl.t
  | D_type of ty_expr Type_decl.t
  | D_module of (module_expr, signature_expr option) Module_decl.t
  | D_module_include of module_expr
  | D_signature of signature_expr Signature_decl.t

and declaration = declaration_content Location.wrap
and decl = declaration [@@deriving eq, compare, yojson, hash]
and module_expr_content = decl Module_expr.t
and module_expr = module_expr_content Location.wrap [@@deriving eq, compare, yojson, hash]

and sig_item =
  | S_value of Value_var.t * ty_expr * sig_item_attribute
  | S_type of Type_var.t * ty_expr
  | S_type_var of Type_var.t
  | S_module of Module_var.t * signature
  | S_module_type of Module_var.t * signature

and sig_item_attribute =
  { entry : bool
  ; view : bool
  ; dyn_entry : bool
  }

and signature = { items : sig_item list }

and signature_content =
  | S_sig of signature
  | S_path of Module_var.t Simple_utils.List.Ne.t

and signature_expr = signature_content Location.wrap

type module_ = decl list [@@deriving eq, compare, yojson, hash]
type program = declaration list [@@deriving eq, compare, yojson, hash]

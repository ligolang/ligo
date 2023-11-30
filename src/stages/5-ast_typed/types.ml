open Ligo_prim
module Location = Simple_utils.Location
module Row = Row.With_layout

type type_variable = Type_var.t [@@deriving compare, hash]
type expression_variable = Value_var.t [@@deriving compare, hash]
type module_variable = Module_var.t [@@deriving compare, hash]

type type_content =
  | T_variable of Type_var.t
  | T_constant of type_injection
  | T_sum of type_expression Row.t
  | T_record of type_expression Row.t
  | T_arrow of ty_expr Arrow.t
  | T_singleton of Literal_value.t
  | T_abstraction of ty_expr Abstraction.t
  | T_for_all of ty_expr Abstraction.t

and type_injection =
  { language : string
  ; injection : Ligo_prim.Literal_types.t
  ; parameters : ty_expr list
  }

and row = ty_expr Row.t
and te_list = type_expression list
and annot_option = string option
and row_element = ty_expr Row.t

and type_expression =
  { type_content : type_content
  ; orig_var : Type_var.t option [@eq.ignore] [@hash.ignore] [@compare.ignore]
  ; location : Location.t [@eq.ignore] [@hash.ignore] [@compare.ignore]
  }

and ty_expr = type_expression [@@deriving eq, compare, yojson, hash]

module ValueAttr = Value_attr
module TypeOrModuleAttr = Type_or_module_attr
module SignatureAttr = Signature_attr
module Access_label = Access_label
module Accessor = Accessor (Access_label)
module Update = Update (Access_label)
module Value_decl = Value_decl (ValueAttr)
module Type_decl = Type_decl (TypeOrModuleAttr)
module Module_decl = Module_decl (TypeOrModuleAttr)
module Signature_decl = Signature_decl (SignatureAttr)
module Pattern = Linear_pattern
module Match_expr = Match_expr.Make (Pattern)
module Let_in = Let_in.Make (Pattern) (ValueAttr)
module Pattern_decl = Pattern_decl (Pattern) (ValueAttr)

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_contract of Module_var.t Simple_utils.List.Ne.t
  | E_literal of Literal_value.t
  | E_constant of
      expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr) Lambda.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_let_in of (expr, ty_expr) Let_in.t
  | E_mod_in of (expr, module_expr) Mod_in.t
  | E_raw_code of expr Raw_code.t
  | E_type_inst of type_inst
  | E_type_abstraction of expr Type_abs.t
  | E_coerce of (expr, ty_expr) Ascription.t
  (* Variant *)
  | E_constructor of expr Constructor.t (* For user defined constructors *)
  | E_matching of (expr, ty_expr) Match_expr.t
  (* Record *)
  | E_record of expr Record.t
  | E_accessor of expr Accessor.t
  | E_update of expr Update.t
  | E_module_accessor of Value_var.t Module_access.t
  (* Imperative *)
  | E_let_mut_in of (expr, ty_expr) Let_in.t
  | E_assign of (expr, ty_expr) Assign.t
  | E_deref of Value_var.t
  | E_for of expr For_loop.t
  | E_for_each of expr For_each_loop.t
  | E_while of expr While_loop.t

and type_inst =
  { forall : expression
  ; type_ : type_expression
  }

and expression =
  { expression_content : expression_content
  ; location : Location.t [@hash.ignore]
  ; type_expression : type_expression
  }

and expr = expression [@@deriving eq, compare, yojson, hash]

and declaration_content =
  | D_value of (expr, ty_expr) Value_decl.t
  | D_irrefutable_match of (expr, ty_expr) Pattern_decl.t
  | D_type of ty_expr Type_decl.t
  | D_module of (module_expr, unit) Module_decl.t
  | D_module_include of module_expr
  | D_signature of signature Signature_decl.t

and declaration = declaration_content Location.wrap
and decl = declaration [@@deriving eq, compare, yojson, hash]
and module_content = decl Module_expr.t [@@deriving eq, compare, yojson, hash]

and module_expr =
  { module_content : module_content
  ; module_location : Location.t [@eq.ignore] [@hash.ignore]
  ; signature : signature
  }
[@@deriving eq, compare, yojson, hash]

and sig_item =
  | S_value of Value_var.t * ty_expr * Sig_item_attr.t
  | S_type of Type_var.t * ty_expr * Sig_type_attr.t
  | S_type_var of Type_var.t * Sig_type_attr.t
  | S_module of Module_var.t * signature
  | S_module_type of Module_var.t * signature

and signature =
  { sig_items : sig_item list
  ; sig_sort : signature_sort
  }

and contract_sig =
  { storage : ty_expr
  ; parameter : ty_expr
  }

and signature_sort =
  | Ss_module
  | Ss_contract of contract_sig

type module_ = decl list [@@deriving eq, compare, yojson, hash]

type program =
  { pr_module : module_
  ; pr_sig : signature
  }
[@@deriving eq, compare, yojson, hash]

open Ligo_prim
module Location = Simple_utils.Location
module Ne_list = Simple_utils.Ne_list
module Row = Row.With_layout

type type_variable = Type_var.t [@@deriving compare, hash]
type expression_variable = Value_var.t [@@deriving compare, hash]
type module_variable = Module_var.t [@@deriving compare, hash]

type type_content =
  | T_variable of Type_var.t
  | T_exists of Type_var.t
  | T_constant of type_injection
  | T_sum of type_expression Row.t
  | T_union of type_expression Union.t
  | T_record of type_expression Row.t
  | T_arrow of type_expression Arrow.t
  | T_singleton of Literal_value.t
  | T_abstraction of type_expression Abstraction.t
  | T_for_all of type_expression Abstraction.t

and type_injection =
  { language : string
  ; injection : Ligo_prim.Literal_types.t
  ; parameters : type_expression list
  }

and row = type_expression Row.t
and te_list = type_expression list
and annot_option = string option
and row_element = type_expression Row.t

and abbrev =
  { orig_var : Module_var.t list * Type_var.t
  ; applied_types : type_expression list
        (** [orig_var] doesn't remember type parameters,
            so to be able using the [orig_var] for parametric types we store the params here. *)
  }

and type_expression =
  { type_content : type_content
  ; abbrev : abbrev option [@equal.ignore] [@hash.ignore] [@compare.ignore]
  ; location : Location.t [@equal.ignore] [@hash.ignore] [@compare.ignore]
  ; source_type : type_expression option [@equal.ignore] [@compare.ignore] [@hash.ignore]
        (* Used in Ast_aggregated *)
  }
[@@deriving equal, compare, yojson, hash, bin_io]

type ty_expr = type_expression [@@deriving equal, compare, yojson, hash, bin_io]

type sig_item_content =
  | S_value of Value_var.t * ty_expr * Sig_item_attr.t
  | S_type of Type_var.t * ty_expr * Sig_type_attr.t
  | S_type_var of Type_var.t * Sig_type_attr.t
  | S_module of Module_var.t * signature
  | S_module_type of Module_var.t * signature

and sig_item = sig_item_content Location.wrap

and signature =
  { sig_items : sig_item list
  ; sig_sort : signature_sort
  }
[@@deriving equal, compare, yojson, hash, bin_io]

and contract_sig =
  { storage : ty_expr
  ; parameter : ty_expr
  }

and signature_sort =
  | Ss_module
  | Ss_contract of contract_sig

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
module Import_decl = Import_decl (Type_or_module_attr)

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_contract of Module_var.t Ne_list.t
  | E_literal of Literal_value.t
  | E_constant of
      expression Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expression Application.t
  | E_lambda of (expression, ty_expr) Lambda.t
  | E_recursive of (expression, ty_expr) Recursive.t
  | E_let_in of (expression, ty_expr) Let_in.t
  | E_mod_in of (expression, module_expr) Mod_in.t
  | E_raw_code of expression Raw_code.t
  | E_type_inst of type_inst
  | E_type_abstraction of expression Type_abs.t
  | E_coerce of (expression, ty_expr) Ascription.t
  (* Variant *)
  | E_constructor of expression Constructor.t (* For user defined constructors *)
  | E_matching of (expression, ty_expr) Match_expr.t
  (* Unions *)
  | E_union_injected of (expression, ty_expr) Union.Injected.t
  | E_union_match of (expression, ty_expr) Union.Match.t
  | E_union_use of expression Union.Use.t
  (* Record *)
  | E_record of expression Record.t
  | E_accessor of expression Accessor.t
  | E_update of expression Update.t
  | E_module_accessor of Value_var.t Module_access.t
  (* Imperative *)
  | E_let_mut_in of (expression, ty_expr) Let_in.t
  | E_assign of (expression, ty_expr) Assign.t
  | E_deref of Value_var.t
  | E_for of expression For_loop.t
  | E_for_each of expression For_each_loop.t
  | E_while of expression While_loop.t
  (* Error recovery *)
  | E_error of Ast_core.expression

and type_inst =
  { forall : expression
  ; type_ : type_expression
  }

and expression =
  { expression_content : expression_content
  ; location : Location.t [@equal.ignore] [@hash.ignore] [@compare.ignore]
  ; type_expression : type_expression
  }

and declaration_content =
  | D_value of (expression, ty_expr) Value_decl.t
  | D_irrefutable_match of (expression, ty_expr) Pattern_decl.t
  | D_type of ty_expr Type_decl.t
  | D_module of (module_expr, unit) Module_decl.t
  | D_module_include of module_expr
  | D_signature of signature Signature_decl.t
  | D_import of Import_decl.t
[@@deriving equal, compare, yojson, hash]

and declaration = declaration_content Location.wrap [@@deriving bin_io]
and module_content = declaration Module_expr.t [@@deriving equal, compare, yojson, hash]

and module_expr =
  { module_content : module_content
  ; module_location : Location.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
  ; signature : signature
  }
[@@deriving equal, compare, yojson, hash]

type expr = expression [@@deriving equal, compare, yojson, hash, bin_io]

type decl = declaration [@@deriving equal, compare, yojson, hash, bin_io]

type module_ = decl list [@@deriving equal, compare, yojson, hash, bin_io]

type program =
  { pr_module : module_
  ; pr_sig : signature
  }
[@@deriving equal, compare, yojson, hash]

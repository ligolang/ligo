[@@@warning "-30"]

open Ligo_prim
module Location = Simple_utils.Location

type type_content =
  | T_variable of Type_var.t
  | T_constant of type_injection
  | T_sum of rows
  | T_record of rows
  | T_arrow of ty_expr Arrow.t
  | T_singleton of Literal_value.t
  | T_for_all of ty_expr Abstraction.t

and type_injection =
  { language : string
  ; injection : Ligo_prim.Literal_types.t
  ; parameters : ty_expr list
  }

and rows =
  { fields : row_element Record.t
  ; layout : Layout.t
  }

and te_list = type_expression list
and annot_option = string option
and row_element = type_expression Rows.row_element_mini_c

and type_expression =
  { type_content : type_content
  ; source_type : Ast_typed.type_expression option
        [@equal.ignore] [@compare.ignore] [@hash.ignore]
  ; location : Location.t [@equal.ignore] [@compare.ignore] [@hash.ignore]
  ; orig_var : Type_var.t option [@equal.ignore] [@compare.ignore] [@hash.ignore]
  }

and type_expression_list = type_expression list
and ty_expr = type_expression [@@deriving equal, compare, yojson, hash]

module ValueAttr = Ast_typed.ValueAttr
module ModuleAttr = Ast_typed.TypeOrModuleAttr
module Value_decl = Value_decl (ValueAttr)

module Access_label = struct
  type 'a t = Label.t

  let equal _ = Label.equal
  let compare _ = Label.compare
  let to_yojson _ = Label.to_yojson
  let of_yojson _ = Label.of_yojson
  let hash_fold_t _ = Label.hash_fold_t
  let pp _ = Label.pp
  let fold _ = Fun.const
  let map _ = Fun.id
  let fold_map _ a b = a, b
end

module Pattern = Linear_pattern
module Accessor = Accessor (Access_label)
module Update = Update (Access_label)
module Let_in = Let_in.Make (Pattern) (ValueAttr)
module Match_expr = Match_expr.Make (Pattern)
module Pattern_decl = Pattern_decl (Pattern) (ValueAttr)

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_literal of Literal_value.t
  | E_constant of
      expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr) Lambda.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_let_in of (expr, ty_expr) Let_in.t
  | E_raw_code of expr Raw_code.t
  | E_type_inst of type_inst
  | E_type_abstraction of expr Type_abs.t
  (* Variant *)
  | E_constructor of expr Constructor.t (* For user defined constructors *)
  | E_matching of (expr, ty_expr) Match_expr.t
  (* Record *)
  | E_record of expr Record.t
  | E_accessor of expr Accessor.t
  | E_update of expr Update.t
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

and expr = expression

and declaration_content =
  | D_value of (expr, ty_expr) Value_decl.t
  | D_irrefutable_match of (expr, ty_expr) Pattern_decl.t
[@@deriving eq, compare, yojson, hash]

and declaration = declaration_content Location.wrap
and decl = declaration [@@deriving eq, compare, yojson, hash]

type context = declaration list [@@deriving eq, compare, yojson, hash]
type program = context * expression

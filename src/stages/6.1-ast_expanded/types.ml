[@@@warning "-30"]

module Location = Simple_utils.Location
open Ligo_prim
module Row = Row.With_layout

type type_expression = [%import: Ast_aggregated.type_expression]

and type_content = [%import: Ast_aggregated.type_content]
[@@deriving equal, compare, yojson, hash]

and ty_expr = type_expression

type row = [%import: Ast_aggregated.row]

module ValueAttr = struct
  include Ast_aggregated.ValueAttr
end

module ModuleAttr = struct
  include Ast_aggregated.ModuleAttr
end

module Value_decl = Value_decl (ValueAttr)

module Access_label = struct
  include Ast_aggregated.Access_label
end

module Accessor = Ast_aggregated.Accessor
module Update = Ast_aggregated.Update
module Let_in = Let_in.Make (Binder) (ValueAttr)

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
  | E_matching of matching
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

and 'e matching_content_case =
  { constructor : Label.t
  ; pattern : Value_var.t
  ; body : 'e
  }

and 'e matching_content_case_list = 'e matching_content_case list

and 'e matching_content_variant =
  { cases : 'e matching_content_case_list
  ; tv : type_expression
  }
[@@deriving eq, compare, yojson, hash]

and 'e matching_content_record =
  { fields : type_expression Binder.t Record.t
  ; body : 'e
  ; tv : type_expression
  }
[@@deriving eq, compare, yojson, hash]

and matching_expr =
  | Match_variant of expr matching_content_variant
  | Match_record of expr matching_content_record

and matching =
  { matchee : expression
  ; cases : matching_expr
  }

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

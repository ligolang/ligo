open Ligo_prim
module Location = Simple_utils.Location
module List = Simple_utils.List

type type_content =
  | T_variable        of TypeVar.t
  | T_sum             of ty_expr Rows.t
  | T_record          of ty_expr Rows.t
  | T_tuple           of ty_expr list
  | T_arrow           of ty_expr Arrow.t
  | T_app             of ty_expr Type_app.t
  | T_module_accessor of TypeVar.t Module_access.t
  | T_singleton       of Literal_value.t
  | T_abstraction     of ty_expr Abstraction.t
  | T_for_all         of ty_expr Abstraction.t

and type_expression = {
  type_content: type_content;
  location: Location.t; [@hash.ignore]
  }
and ty_expr = type_expression
  [@@deriving eq,compare,yojson,hash]

type attributes = string list
  [@@deriving eq,compare,yojson,hash]
let pp_attributes ppf lst =
  Format.fprintf ppf "%a" Simple_utils.PP_helpers.(list_sep_d String.pp) lst
(* | Macro_declaration of macro_declaration *)
module Attr = struct
  type value = attributes
  and type_ = attributes
  and module_ = attributes
    [@@deriving eq,compare,yojson,hash]
  let pp_value = pp_attributes
  let pp_type  = pp_attributes
  let pp_module = pp_attributes
end

module Declaration=Declaration(Attr)

module Accessor = Accessor(Access_path)
module Update   = Update(Access_path)
type expression_content =
  (* Base *)
  | E_variable of ValueVar.t
  | E_literal of Literal_value.t
  | E_constant of expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr option) Lambda.t
  | E_type_abstraction of expr Type_abs.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_let_in of (expr, ty_expr option) Let_in.t
  | E_type_in of (expr, ty_expr) Type_in.t
  | E_mod_in of (expr, module_expr) Mod_in.t
  | E_raw_code  of expr Raw_code.t
  (* Variant *)
  | E_constructor of expr Constructor.t (* For user defined constructors *)
  | E_matching of (expr, ty_expr option) Match_expr.t
  (* Record *)
  | E_record of expr Record.t
  | E_accessor of expr Accessor.t
  | E_update   of expr Update.t
  (* Advanced *)
  | E_ascription of (expr, ty_expr) Ascription.t
  | E_module_accessor of ValueVar.t Module_access.t
  (* Sugar *)
  | E_cond of expr Conditional.t
  | E_sequence of expr Sequence.t
  | E_skip of Skip.t
  | E_tuple of expr list
  (* Data Structures *)
  | E_map     of expr Map_expr.t
  | E_big_map of expr Map_expr.t
  | E_list    of expr List_expr.t
  | E_set     of expr Set_expr.t
  (* Imperative *)
  | E_assign   of (expr,ty_expr option) Assign.t


and expression = {
  expression_content: expression_content;
  location: Location.t; [@hash.ignore]
  }

and expr = expression
  [@@deriving eq,compare,yojson,hash]



and declaration_content = (expr,ty_expr,decl) Declaration.declaration
and  declaration = declaration_content Location.wrap
and  decl = Decl of declaration
  [@@deriving eq,compare,yojson,hash]

and module_expr_content = decl Module_expr.t
and module_expr = module_expr_content Location.wrap
  [@@deriving eq,compare,yojson,hash]

type module_ = decl list
  [@@deriving eq,compare,yojson,hash]

type program = declaration list
  [@@deriving eq,compare,yojson,hash]


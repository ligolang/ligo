open Ligo_prim
module Location = Simple_utils.Location
module List = Simple_utils.List
module Ligo_string = Simple_utils.Ligo_string


type sugar_type_expression_option = Ast_sugar.type_expression option
  [@@deriving eq,compare,yojson,hash]
type sugar_expression_option = Ast_sugar.expression option
  [@@deriving eq,compare,yojson,hash]

type string_option = string option


type type_content =
  | T_variable        of TypeVar.t
  | T_sum             of rows
  | T_record          of rows
  | T_arrow           of ty_expr Arrow.t
  | T_app             of ty_expr Type_app.t
  | T_module_accessor of TypeVar.t Module_access.t
  | T_singleton       of Literal_value.t
  | T_abstraction     of ty_expr Abstraction.t
  | T_for_all         of ty_expr Abstraction.t

and rows = {
  fields : row_element Record.t ;
  layout : Layout.t option
}

and row_element = ty_expr Rows.row_element_mini_c

and type_expression = {
  type_content  : type_content ;
  sugar    : sugar_type_expression_option [@deriving.ignore];
  location : Location.t [@deriving.ignore] [@hash.ignore];
  }
and ty_expr = type_expression
  [@@deriving eq,compare,yojson,hash]

and type_expression_option = type_expression option
  [@@deriving eq,compare,yojson,hash]
module Attr = struct
  type value = {
    inline: bool ;
    no_mutation: bool;
    (* Some external constant (e.g. `Test.balance`) do not accept any argument. This annotation is used to prevent LIGO interpreter to evaluate (V_Thunk values) and forces inlining in the compiling (15-self_mini_c)
      TODO: we should change the type of such constants to be `unit -> 'a` instead of just 'a
    *)
    view : bool;
    public: bool;
    (* Controls whether a declaration must be printed or not when using LIGO print commands (print ast-typed , ast-aggregated .. etc ..)
      set to true for standard libraries
    *)
    hidden: bool;
    (* Controls whether it should be inlined at AST level *)
    thunk: bool ;
  } [@@deriving eq,compare,yojson,hash]
  type type_ = { public: bool ; hidden : bool }
    [@@deriving eq,compare,yojson,hash]
  type module_ = type_
    [@@deriving eq,compare,yojson,hash]

  open Format
  let pp_if_set str ppf attr =
    if attr then
      fprintf ppf "[@@%s]" str
    else
      fprintf ppf ""
  let pp_value ppf { inline ; no_mutation ; view ; public ; hidden ; thunk } =
    fprintf ppf "%a%a%a%a%a%a"
      (pp_if_set "inline") inline
      (pp_if_set "no_mutation") no_mutation
      (pp_if_set "view") view
      (pp_if_set "private") (not public)
      (pp_if_set "hidden") hidden
      (pp_if_set "thunk") thunk

  let pp_type ppf { public ; hidden } =
    fprintf ppf "%a%a"
      (pp_if_set "private") (not public)
      (pp_if_set "hidden") hidden
  let pp_module = pp_type
end

module Declaration=Declaration(Attr)
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
  let fold_map _ = fun a b -> a,b
end
module Accessor = Accessor(Access_label)
module Update   = Update(Access_label)
type expression_content =
  (* Base *)
  | E_variable of ValueVar.t
  | E_literal of Literal_value.t
  | E_constant of expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr option) Lambda.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_type_abstraction of expr Type_abs.t
  | E_let_in of let_in
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
  (* Imperative *)
  | E_assign   of (expr,ty_expr option) Assign.t

and let_in = {
    let_binder: ty_expr option Binder.t ;
    rhs: expression ;
    let_result: expression ;
    attr: Attr.value ;
  }
and expression = {
  expression_content  : expression_content ;
  sugar    : sugar_expression_option ;
  location : Location.t ; [@hash.ignore]
  }
and expr = expression
  [@@deriving eq,compare,yojson,hash]

and declaration_content = (expr,ty_expr,decl) Declaration.declaration
and declaration = declaration_content Location.wrap
and decl = Decl of declaration
  [@@deriving eq,compare,yojson,hash]

and module_expr_content = decl Module_expr.t
and module_expr = module_expr_content Location.wrap
  [@@deriving eq,compare,yojson,hash]

type module_ = decl list
  [@@deriving eq,compare,yojson,hash]

type program = declaration list
  [@@deriving eq,compare,yojson,hash]


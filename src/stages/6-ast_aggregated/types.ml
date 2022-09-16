[@@@warning "-30"]

open Ligo_prim
module Location = Simple_utils.Location


type type_content =
  | T_variable  of Type_var.t
  | T_constant  of type_injection
  | T_sum       of rows
  | T_record    of rows
  | T_arrow     of ty_expr Arrow.t
  | T_singleton of Literal_value.t
  | T_for_all   of ty_expr Abstraction.t

and type_injection = {
  language : string ;
  injection : Ligo_prim.Literal_types.t ;
  parameters : ty_expr list ;
}

and rows = {
  fields : row_element Record.t ;
  layout : Layout.t
}
and te_list = type_expression list

and annot_option = string option

and row_element = type_expression Rows.row_element_mini_c

and type_expression = {
    type_content : type_content;
    source_type : Ast_typed.type_expression option [@equal.ignore] [@compare.ignore] [@hash.ignore] ;
    location : Location.t [@equal.ignore] [@compare.ignore] [@hash.ignore] ;
    orig_var : Type_var.t option [@equal.ignore] [@compare.ignore] [@hash.ignore] ;
  }
and type_expression_list = type_expression list
and ty_expr = type_expression
  [@@deriving equal,compare,yojson,hash]

module ValueAttr = struct
  type t = {
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
  open Format
  let pp_if_set str ppf attr =
    if attr then fprintf ppf "[@@%s]" str
    else fprintf ppf ""

  let pp ppf { inline ; no_mutation ; view ; public ; hidden ; thunk } =
    fprintf ppf "%a%a%a%a%a%a"
      (pp_if_set "inline") inline
      (pp_if_set "no_mutation") no_mutation
      (pp_if_set "view") view
      (pp_if_set "private") (not public)
      (pp_if_set "hidden") hidden
      (pp_if_set "thunk") thunk

end

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


type 'e matching_content_case = {
    constructor : Label.t ;
    pattern : Value_var.t ;
    body : 'e ;
  }

and 'e matching_content_case_list = 'e matching_content_case list

and 'e matching_content_variant = {
    cases: 'e matching_content_case_list;
    tv: type_expression;
  } [@@deriving eq,compare,yojson,hash]

type 'e matching_content_record = {
  fields : type_expression Binder.t Record.LMap.t;
  body : 'e;
  tv : type_expression;
} [@@deriving eq,compare,yojson,hash]

type expression_content =
  (* Base *)
  | E_variable of Value_var.t
  | E_literal of Literal_value.t
  | E_constant of expr Constant.t (* For language constants, like (Cons hd tl) or (plus i j) *)
  | E_application of expr Application.t
  | E_lambda of (expr, ty_expr) Lambda.t
  | E_recursive of (expr, ty_expr) Recursive.t
  | E_let_in    of let_in
  | E_raw_code  of expr Raw_code.t
  | E_type_inst of type_inst
  | E_type_abstraction of expr Type_abs.t
  (* Variant *)
  | E_constructor of expr Constructor.t (* For user defined constructors *)
  | E_matching of matching
  (* Record *)
  | E_record of expr Record.t
  | E_accessor of expr Accessor.t
  | E_update   of expr Update.t
  | E_assign   of (expr,ty_expr) Assign.t

and type_inst = {
    forall: expression ;
    type_: type_expression ;
  }

and let_in = {
    let_binder: ty_expr Binder.t ;
    rhs: expression ;
    let_result: expression ;
    attr: ValueAttr.t ;
  }

and matching_expr =
  | Match_variant of expr matching_content_variant
  | Match_record  of expr matching_content_record

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }

and expression = {
    expression_content: expression_content ;
    location: Location.t [@hash.ignore] ;
    type_expression: type_expression ;
  }

and expr = expression
  [@@deriving eq,compare,yojson,hash]


type 'a program = 'a -> expression

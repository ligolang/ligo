[@@@warning "-30"]

module Nano_prim = Nano_prim
module Location = Simple_utils.Location
module List = Simple_utils.List
module Label = Ligo_prim.Label
module Variable = Ligo_prim.Value_var
module Ty_variable = Ligo_prim.Type_var
module Mod_variable = Ligo_prim.Module_var
module Module_access = Ligo_prim.Module_access
module Literal_value = Ligo_prim.Literal_value
module Raw_code = Ligo_prim.Raw_code
module Constant = Ligo_prim.Constant
module Constructor = Ligo_prim.Constructor
module Lambda = Ligo_prim.Lambda
module Application = Ligo_prim.Application
module Record = Ligo_prim.Record
module Non_linear_rows = Nano_prim.Non_linear_rows (Label)
module Field = Nano_prim.Field
module Array_repr = Nano_prim.Array_repr
module Object_ = Nano_prim.Object_
module Selection = Nano_prim.Selection
module Match_tc39 = Nano_prim.Match_tc39

(* module Projection = Ligo_prim.Accessor (Nano_prim.Selection) *)
module Update = Nano_prim.Update
module Param = Nano_prim.Param
module Poly_fun = Nano_prim.Poly_fun
module Map_lookup = Nano_prim.Map_lookup
module Type_in = Nano_prim.Type_in
module Mod_in = Nano_prim.Mod_in
module Block_with = Nano_prim.Block_with
module Assign = Ligo_prim.Assign
module Assign_chainable = Nano_prim.Assign_chainable
module Prefix_postfix = Nano_prim.Prefix_postfix
module Type_decl = Nano_prim.Type_decl
module Type_abstraction = Nano_prim.Type_abstraction
module Record_update = Nano_prim.Record_update
module Record_access = Nano_prim.Record_access
module Pattern_decl = Nano_prim.Pattern_decl
module Simple_let_in = Nano_prim.Simple_let_in
module Recursive = Nano_prim.Recursive
module Abstraction = Ligo_prim.Abstraction
module Abstractions = Ligo_prim.Abstractions
module Row = Ligo_prim.Row.With_optional_layout

module Empty_label = struct
  type t = unit [@@deriving eq, compare, yojson, iter, hash, sexp]

  let of_string _ = ()
  let to_string () = "unlabeled"
end

module Non_linear_disc_rows = Nano_prim.Non_linear_rows (Empty_label)
module Attribute = Nano_prim.Attribute
module Named_fun = Nano_prim.Named_fun
module Type_app = Nano_prim.Type_app
module Arrow = Nano_prim.Arrow
module Mod_access = Nano_prim.Mod_access
module Struct_assign = Nano_prim.Struct_assign
module Instruction_call = Nano_prim.Instruction_call
module Case = Nano_prim.Case
module Test_clause = Nano_prim.Test_clause
module Cond = Nano_prim.Cond
module For_int = Nano_prim.For_int
module For_collection = Nano_prim.For_collection
module Patch = Nano_prim.Patch
module For_of = Nano_prim.For_of
module For_stmt = Nano_prim.For_stmt
module Removal = Nano_prim.Removal
module While = Nano_prim.While
module Switch = Nano_prim.Switch
module Import = Nano_prim.Import
module Let_decl = Nano_prim.Let_decl
module Simple_decl = Nano_prim.Simple_decl
module Fun_decl = Nano_prim.Fun_decl
module Type_abstraction_decl = Nano_prim.Type_abstraction_decl
module Mod_decl = Nano_prim.Mod_decl
module Sig_decl = Nano_prim.Sig_decl
module Operators = Nano_prim.Operators
module Let_binding = Nano_prim.Let_binding
module Rev_app = Nano_prim.Rev_app
module Z = Ligo_prim.Literal_value.Z
module Ty_escaped_var = Nano_prim.Ty_escaped_var
module Value_escaped_var = Nano_prim.Value_escaped_var

(*  INFO: Node tagged with [@not_initial] should not be emited by unification
   the first nanopass do the check *)

(* ========================== TYPES ======================================== *)

type 'self type_expression_ = 'self type_expression_content_ Location.wrap
and 'self ty_expr_ = 'self type_expression_

and 'self type_expression_content_ =
  | T_attr of Attribute.t * 'self (* [@a] x *)
  | T_var of Ty_variable.t [@not_initial] (* x *)
  | T_var_esc of Ty_escaped_var.t (* x or @x without @ *)
  | T_contract_parameter of Mod_variable.t Simple_utils.List.Ne.t
  | T_constant of string [@not_initial]
  | T_prod of 'self Simple_utils.List.Ne.t (* x * y *)
  | T_app of ('self, 'self) Type_app.t (* x y *)
  | T_fun of 'self Arrow.t (* Initial in CameLIGO only: x -> y *)
  | T_named_fun of 'self Named_fun.t (* JsLIGO only: (x : x) => y *)
  | T_string of string (* Some types are parametrized by strings / ints *)
  | T_int of string * Z.t
  | T_module_open_in of (Mod_variable.t, 'self) Mod_access.t (* A.(<...>) *)
  | T_arg of string (* 'a *)
  | T_sum_raw of
      'self option Non_linear_rows.t (* A of int | B of string , initial for CameLIGO*)
  | T_record_raw of 'self option Non_linear_rows.t (* {a: int; b : int} *)
  | T_disc_union of 'self Non_linear_disc_rows.t
    (* { kind: "A" } | { kind: "B" }, initial for JsLIGO only *)
  | T_abstraction of 'self Abstraction.t [@not_initial]
    (* [type 'a t = 'a * 'a] <- [t] this decl will be replaced by an abstraction
       at passes/04-nanopasses/passes/type_abstraction_declaration.ml *)
  | T_module_access of
      (Mod_variable.t Simple_utils.List.Ne.t, Ty_variable.t) Mod_access.t (* A.B.x *)
  | T_module_app of
      ( (Mod_variable.t Simple_utils.List.Ne.t, Ty_variable.t) Mod_access.t
      , 'self )
      Type_app.t [@not_initial]
  | T_sum of 'self Row.t [@not_initial]
  | T_record of 'self Row.t [@not_initial]
  | T_for_all of 'self Abstraction.t [@not_initial]
  | T_for_alls of 'self Abstractions.t
[@@deriving
  map
  , fold
  , yojson
  , iter
  , sexp
  , is { tags = [ "not_initial" ]; name = "ty_expr" }
  , eq
  , compare
  , hash]

(* ========================== PATTERNS ===================================== *)
type ('self, 'ty_expr) pattern_ = ('self, 'ty_expr) pattern_content_ Location.wrap

and 'self list_pattern =
  | Cons of 'self * 'self
  | List of 'self list

and 'self element_pattern =
  { ellipsis : bool
  ; pattern : 'self
  }

and ('self, 'ty_expr) pattern_content_ =
  | P_unit
  | P_typed of 'ty_expr * 'self
  | P_literal of Literal_value.t
  | P_var of Variable.t [@not_initial]
  | P_var_esc of Value_escaped_var.t (* x or @x without @ *)
  | P_list of 'self list_pattern
  | P_variant of Label.t * 'self option
  | P_tuple of 'self list
  | P_tuple_with_ellipsis of 'self element_pattern list
  | P_pun_record of (Label.t, 'self) Field.t list
  | P_rest of Label.t
  | P_attr of Attribute.t * 'self
  | P_mod_access of (Mod_variable.t Simple_utils.List.Ne.t, 'self) Mod_access.t
  | P_app of 'self * 'self option
  | P_ctor of Label.t
  | P_ctor_app of 'self Simple_utils.List.Ne.t
  | P_var_typed of 'ty_expr * Variable.t [@not_initial]
[@@deriving
  map
  , fold
  , yojson
  , iter
  , sexp
  , is { tags = [ "not_initial" ]; name = "pattern" }
  , eq
  , compare
  , hash]

(* ========================== INSTRUCTIONS ================================= *)
type ('self, 'expr, 'pattern, 'statement, 'block) instruction_ =
  ('self, 'expr, 'pattern, 'statement, 'block) instruction_content_ Location.wrap

and ('self, 'expr, 'pattern, 'statement, 'block) instruction_content_ =
  | I_struct_assign of 'expr Struct_assign.t
  | I_call of 'expr Instruction_call.t
  | I_case of ('expr, 'pattern, ('self, 'block) Test_clause.t) Case.t
  | I_cond of ('expr, ('self, 'block) Test_clause.t) Cond.t
  | I_for of ('expr, 'block) For_int.t
  | I_for_in of ('pattern, 'expr, 'block) For_collection.t
  | I_for_of of ('expr, 'statement) For_of.t
  | I_for_stmt of ('expr, 'statement) For_stmt.t
  | I_patch of 'expr Patch.t
  | I_remove of 'expr Removal.t
  | I_skip
  | I_while of ('expr, 'block) While.t
  | I_block of 'block
  | I_expr of 'expr
  | I_return of 'expr option [@sexp.option]
  | I_switch of ('expr, 'block) Switch.t
  | I_break
  | I_continue
  | I_assign of Variable.t * 'expr [@not_initial]
[@@deriving
  map
  , fold
  , yojson
  , iter
  , sexp
  , is { tags = [ "not_initial" ]; name = "instruction" }
  , eq
  , compare
  , hash]

(* ========================== STATEMENTS ========================= *)
type ('self, 'instruction, 'declaration) statement_ =
  ('self, 'instruction, 'declaration) statement_content_ Location.wrap

and ('self, 'instruction, 'declaration) statement_content_ =
  | S_attr of (Attribute.t * 'self)
  | S_export of 'declaration
  | S_instr of 'instruction
  | S_decl of 'declaration
  | S_directive of unit (* directive ignored for now *)
[@@deriving
  map
  , fold
  , yojson
  , iter
  , sexp
  , is { tags = [ "not_initial" ]; name = "statement" }
  , eq
  , compare
  , hash]

(* ========================== BLOCKS ======================================= *)

include struct
  [@@@warning "-27"]

  type ('self, 'statement) block_ = 'statement Simple_utils.List.Ne.t Location.wrap
  [@@deriving map, fold, yojson, iter, sexp, eq, compare, hash]
end

(* ========================== DECLARATIONS ================================= *)
type ('self, 'expr, 'ty_expr, 'pattern, 'mod_expr, 'sig_expr) declaration_ =
  ('self, 'expr, 'ty_expr, 'pattern, 'mod_expr, 'sig_expr) declaration_content_
  Location.wrap

and ('self, 'expr, 'ty_expr, 'pattern, 'mod_expr, 'sig_expr) declaration_content_ =
  | D_attr of (Attribute.t * 'self)
  | D_directive of unit (* directive ignored for now *)
  | D_import of Import.t
  | D_export of 'self
  | D_let of ('expr, 'pattern Simple_utils.List.Ne.t, 'ty_expr) Let_decl.t
    (* let x = <..> ; let x (type a b) y (z:ty) = <..> *)
  | D_var of ('pattern, 'expr, 'ty_expr) Simple_decl.t (* var x = y *)
  | D_multi_var of ('pattern, 'expr, 'ty_expr) Simple_decl.t Simple_utils.List.Ne.t
    (* var x = y , z = w *)
  | D_const of ('pattern, 'expr, 'ty_expr) Simple_decl.t (* const x = y *)
  | D_multi_const of ('pattern, 'expr, 'ty_expr) Simple_decl.t Simple_utils.List.Ne.t
    (* const x = y , z = w *)
  | D_fun of ('ty_expr, 'expr, 'pattern Param.t) Fun_decl.t
  | D_type_abstraction of 'ty_expr Type_abstraction_decl.t
  | D_module of ('mod_expr, 'sig_expr) Mod_decl.t
  | D_module_include of 'mod_expr
  | D_signature of 'sig_expr Sig_decl.t
  | D_type of 'ty_expr Type_decl.t [@not_initial]
  | D_irrefutable_match of ('expr, 'pattern) Pattern_decl.t [@not_initial]
[@@deriving
  map
  , fold
  , yojson
  , iter
  , sexp
  , is { tags = [ "not_initial" ]; name = "declaration" }
  , eq
  , compare
  , hash]

(* ========================== SIGNATURES ====================================== *)
include struct
  [@@@warning "-27"]

  type ('sig_expr, 'sig_entry, 'ty_expr) sig_entry_ =
    ('sig_expr, 'sig_entry, 'ty_expr) sig_entry_content_ Location.wrap

  and ('sig_expr, 'sig_entry, 'ty_expr) sig_entry_content_ =
    | S_value of Variable.t * 'ty_expr * bool
    | S_type of Ligo_prim.Type_var.t * Ligo_prim.Type_var.t list * 'ty_expr
    | S_type_var of Ligo_prim.Type_var.t
    | S_attr of Attribute.t * 'sig_entry
    | S_include of 'sig_expr
  [@@deriving
    map
    , fold
    , yojson
    , iter
    , sexp
    , is { tags = [ "not_initial" ]; name = "sig_entry" }
    , eq
    , compare
    , hash]

  and ('sig_expr, 'sig_entry, 'ty_expr) sig_expr_ =
    ('sig_expr, 'sig_entry, 'ty_expr) sig_expr_content_ Location.wrap

  and ('sig_expr, 'sig_entry, 'ty_expr) sig_expr_content_ =
    | S_body of 'sig_entry list
    | S_path of Ligo_prim.Module_var.t Simple_utils.List.Ne.t
  [@@deriving
    map
    , fold
    , yojson
    , iter
    , sexp
    , is { tags = [ "not_initial" ]; name = "sig_expr" }
    , eq
    , compare
    , hash]
end

(* ========================== MODULES ====================================== *)
include struct
  [@@@warning "-27"]

  type ('self, 'program) mod_expr_ = ('self, 'program) mod_expr_content_ Location.wrap

  and ('self, 'program) mod_expr_content_ =
    | M_body of 'program
    | M_path of Ligo_prim.Module_var.t Simple_utils.List.Ne.t
    | M_var of Ligo_prim.Module_var.t
  [@@deriving
    map
    , fold
    , iter
    , yojson
    , sexp
    , is { tags = [ "not_initial" ]; name = "mod_expr" }
    , eq
    , compare
    , hash]
end

(* ========================== EXPRESSIONS ================================== *)
type ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_ =
  ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_content_ Location.wrap

and ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expr_ =
  ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_

and ('self, 'ty_expr, 'pattern, 'block, 'mod_expr) expression_content_ =
  | E_attr of (Attribute.t * 'self) (* [@a] (x,y)      *)
  | E_literal of Literal_value.t (* 42, 10tez *)
  | E_binary_op of 'self Operators.binary_op
  | E_unary_op of 'self Operators.unary_op
  | E_variable of Variable.t [@not_initial] (* x *)
  | E_variable_esc of Value_escaped_var.t (* x or @x without @ *)
  | E_rev_app of 'self Rev_app.t (* x |> f *)
  | E_tuple of 'self Simple_utils.List.Ne.t (* (x, y, z) *)
  | E_record_pun of (Label.t, 'self) Field.t list (* { x = 10; y; z } *)
  | E_array of
      'self Array_repr.t (* [1, 2, 3] , [42] , [] , [2 ...3] (specific to jsligo) *)
  | E_object of 'self Object_.t (* {a : 1, b : 2}  ; { a ... n } *)
  | E_object_update of 'self Object_.update
  | E_list of 'self list (* [ 1; 2; 3; 4; 5] *)
  | E_proj of 'self * 'self Selection.t list (* x.y.1 *)
  | E_module_open_in of
      (Mod_variable.t Simple_utils.List.Ne.t, 'self) Mod_access.t (* M.N.a or M.N.(a.x)*)
  | E_update of 'self Update.t
  | E_poly_fun of
      ('self, 'ty_expr, 'pattern) Poly_fun.t (* (fun (type a) (x, y) z -> x + y - z) *)
  | E_block_poly_fun of
      ('block, 'ty_expr, 'pattern) Poly_fun.t (* <A>(x: A) => { ... } ) *)
  | E_constr of Label.t
  | E_ctor_app of ('self * 'self Simple_utils.List.Ne.t option)
  | E_applied_constructor of 'self Constructor.t (* MyCtor (42, 43, 44), PascaLigo only *)
  | E_call of 'self * 'self list Location.wrap (* f (x, y) ; f x y *)
  | E_match of ('self, 'pattern, 'self) Case.t (* match e with | A -> ... | B -> ... *)
  | E_match_tc39 of ('self, 'pattern) Match_tc39.t
  | E_annot of ('self * 'ty_expr) (* 42 : int *)
  | E_cond of ('self, 'self) Cond.t (* if b then 42 else 24 *)
  | E_set of 'self list (* set [x; 1] *)
  | E_map_lookup of 'self Map_lookup.t
  | E_map of ('self * 'self) list
  | E_big_map of ('self * 'self) list
  | E_let_in of ('pattern, 'self, 'ty_expr) Let_binding.t (* let x = 42 in x + 1 *)
  | E_type_in of ('self, 'ty_expr) Type_in.t (* type t = int in let x : t = 42 *)
  | E_mod_in of ('self, 'mod_expr) Mod_in.t (* module M = struct let x = 42 end in M.x *)
  | E_raw_code of 'self Raw_code.t
  | E_block_with of ('self, 'block) Block_with.t (* { let x = 1 ; x := 2 } with x *)
  | E_do of 'block
  | E_struct_assign_chainable of
      'self Assign_chainable.structural (* x := y ; which has the type of x/y *)
  | E_let_mut_in of ('pattern, 'self, 'ty_expr) Let_binding.t (* let mut x = 1 *)
  | E_assign_unitary of
      ('self, 'ty_expr option) Assign.t (* x := y ; which has type unit *)
  | E_while of ('self, 'self) While.t
  | E_for of ('self, 'self) For_int.t
  | E_for_in of ('pattern, 'self, 'self) For_collection.t
  | E_sequence of 'self list (* begin a ; a () ; x := y end *)
  | E_contract of Mod_variable.t Simple_utils.List.Ne.t
  | E_constant of 'self Constant.t [@not_initial]
  | E_simple_let_in of ('self, 'pattern) Simple_let_in.t [@not_initial]
  | E_assign_chainable of 'self Assign_chainable.assign [@not_initial]
  | E_poly_recursive of
      ('ty_expr, ('self, 'ty_expr, 'pattern) Poly_fun.t) Recursive.general [@not_initial]
  | E_recursive of ('ty_expr, ('self, 'ty_expr) Lambda.t) Recursive.specialized
      [@not_initial]
  | E_lambda of ('self, 'ty_expr option) Lambda.t [@not_initial]
  | E_type_abstraction of 'self Type_abstraction.t [@not_initial]
  | E_application of 'self Application.t [@not_initial]
  | E_record_update of 'self Record_update.t [@not_initial]
  | E_record_access of 'self Record_access.t [@not_initial]
  | E_module_access of (Mod_variable.t Simple_utils.List.Ne.t, Variable.t) Mod_access.t
      [@not_initial]
  | E_match_block of ('self, 'pattern, 'block) Case.t [@not_initial]
  | E_prefix of 'self Prefix_postfix.prefix
  | E_postfix of 'self Prefix_postfix.postfix
[@@deriving
  map
  , fold
  , iter
  , yojson
  , sexp
  , is { tags = [ "not_initial" ]; name = "expr" }
  , eq
  , compare
  , hash]
(* ========================== PROGRAM ====================================== *)

type ('self, 'declaration, 'instruction) program_entry_ =
  | PE_attr of Attribute.t * 'self
  | PE_declaration of 'declaration
  | PE_top_level_instruction of 'instruction
  | PE_preproc_directive of unit (* directive ignored for now *)
  | PE_export of 'self
[@@deriving
  map
  , fold
  , yojson
  , iter
  , sexp
  , is { tags = [ "not_initial" ]; name = "program_entry" }
  , eq
  , compare
  , hash]

include struct
  [@@@warning "-27"]

  type ('self, 'program_entry) program_ = 'program_entry list
  [@@deriving map, fold, yojson, iter, sexp, eq, compare, hash]
end

type ty_expr = { fp : ty_expr ty_expr_ }
and pattern = { fp : (pattern, ty_expr) pattern_ }
and instruction = { fp : (instruction, expr, pattern, statement, block) instruction_ }
and statement = { fp : (statement, instruction, declaration) statement_ }
and block = { fp : (block, statement) block_ }

and declaration =
  { fp : (declaration, expr, ty_expr, pattern, mod_expr, sig_expr) declaration_ }

and mod_expr = { fp : (mod_expr, program) mod_expr_ }
and expr = { fp : (expr, ty_expr, pattern, block, mod_expr) expr_ }
and program_entry = { fp : (program_entry, declaration, instruction) program_entry_ }
and program = { fp : (program, program_entry) program_ }
and sig_expr = { fp : (sig_expr, sig_entry, ty_expr) sig_expr_ }

and sig_entry = { fp : (sig_expr, sig_entry, ty_expr) sig_entry_ }
[@@deriving eq, compare, hash]

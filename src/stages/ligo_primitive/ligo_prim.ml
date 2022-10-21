(*Primitives *)
module Label = Label
module Binder = Binder
module Param = Param
module Pattern = Pattern
module Access_path = Access_path
module Layout = Layout

(* Type level constructs *)
module Type_var = Var.Type_var
module Literal_types = Literal_types
module Kind  = Kind
module Rows  = Rows
module Arrow = Arrow
module Abstraction = Abstraction
module Type_app = Type_app
module Type_abs = Type_abs
module Layout_var = Var.Layout_var

(* Value level constructs *)
module Value_var = Var.Value_var
module Literal_value = Literal_value
module Constant = Constant
module Lambda = Lambda
module Recursive = Recursive
module Application = Application
module Let_in = Let_in
module Type_in = Type_in
module Raw_code = Raw_code
module Ascription = Ascription
module Constructor = Constructor
module Match_expr = Match_expr
module Record = Record
module Accessor (Path: Access_path.S) = Accessor.Make(Path)
module Update   (Path: Access_path.S) = Update.Make(Path)

(* Sugar *)
module Conditional = Conditional
module Sequence = Sequence
module Skip = Skip

(* Imperative loops *)
module For_loop = For_loop
module For_each_loop = For_each_loop
module While_loop = While_loop
module Assign     = Assign

(* Containers *)
module Map_expr = Map_expr
module Set_expr = Set_expr
module List_expr = List_expr

(* Module language *)
module Module_var    = Var.Module_var
module Module_access = Module_access
module Module_expr   = Module_expr
module Mod_in        = Mod_in

(* Declarations *)
module Value_decl (Attr:Declaration.Attr) = Declaration.Value_decl(Attr)
module Type_decl  (Attr:Declaration.Attr) = Declaration.Type_decl(Attr)
module Module_decl(Attr:Declaration.Attr) = Declaration.Module_decl(Attr)

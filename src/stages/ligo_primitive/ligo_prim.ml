(*Primitives *)
module Label = Label
module Binder = Binder
module Pattern = Pattern
module Access_path = Access_path
module Layout = Layout

(* Type level constructs *)
module TypeVar = Var.TypeVar
module Literal_types = Literal_types
module Kind  = Kind
module Rows  = Rows
module Arrow = Arrow
module Abstraction = Abstraction
module Type_app = Type_app
module Type_abs = Type_abs

(* Value level constructs *)
module ValueVar = Var.ValueVar
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
module Accessor (Access:Accessor.Access) = Accessor.Make(Access)
module Update   (Access:  Update.Access) = Update.Make(Access)

module Conditional = Conditional
module Sequence = Sequence
module Skip = Skip
module For_loop = For_loop
module For_each_loop = For_each_loop
module While_loop = While_loop
module Assign     = Assign

module Map_expr = Map_expr
module Set_expr = Set_expr
module List_expr = List_expr


module ModuleVar = Var.ModuleVar
module Module_access = Module_access
module Module_expr = Module_expr
module Mod_in   = Mod_in

module Declaration(Attr:Declaration.Attr) = Declaration.Make(Attr)

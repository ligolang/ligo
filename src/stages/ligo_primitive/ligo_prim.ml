(*Primitives *)
module Label = Label
module Binder = Binder
module Param = Param
module Linear_pattern = Pattern.Linear_pattern
module Non_linear_pattern = Pattern.Non_linear_pattern
module Access_path = Access_path
module Layout = Layout
module Layout_var = Var.Layout_var
module Magic_vars = Magic_vars

(* Type level constructs *)
module Type_var = Var.Type_var
module Literal_types = Literal_types
module Kind = Kind
module Row = Row
module Arrow = Arrow
module Abstraction = Abstraction
module Abstractions = Abstractions
module Type_app = Type_app
module Type_abs = Type_abs
module Tuple = Tuple

(* Value level constructs *)
module Access_label = Access_label
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
module Accessor (Path : Access_path.S) = Accessor.Make (Path)
module Update (Path : Access_path.S) = Update.Make (Path)

(* Sugar *)
module Conditional = Conditional
module Sequence = Sequence

(* Imperative loops *)
module For_loop = For_loop
module For_each_loop = For_each_loop
module While_loop = While_loop
module Assign = Assign

(* Module language *)
module Module_var = Var.Module_var
module Module_access = Module_access
module Module_expr = Module_expr
module Mod_in = Mod_in

(* Declarations *)
module Value_decl (Attr : Declaration.Attr) = Declaration.Value_decl (Attr)
module Type_decl (Attr : Declaration.Attr) = Declaration.Type_decl (Attr)
module Module_decl (Attr : Declaration.Attr) = Declaration.Module_decl (Attr)
module Signature_decl (Attr : Declaration.Attr) = Declaration.Signature_decl (Attr)
module Value_attr = Value_attr
module Sig_item_attr = Sig_item_attr
module Sig_type_attr = Sig_type_attr
module Type_or_module_attr = Type_or_module_attr
module Signature_attr = Signature_attr

module Pattern_decl (Pattern : Pattern.S) (Attr : Declaration.Attr) =
  Declaration.Pattern_decl (Pattern) (Attr)

(* Misc *)
module Error = Error

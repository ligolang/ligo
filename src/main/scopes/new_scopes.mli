module AST = Ast_core
module Location = Simple_utils.Location
module LSet : Caml.Set.S with type elt = Location.t

module Formatter = New_formatter
module Api_helper = Api_helper

module Types = New_types

type def = Types.def
type scopes = Types.scopes

val scopes : with_types:bool -> options:Compiler_options.middle_end -> AST.module_ -> def list * scopes
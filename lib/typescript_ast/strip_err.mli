(* This module defines all the possible errors for stripping the AST down *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* LIGO dependencies *)

module Wrap = Lexing_shared.Wrap

(* Errors *)

type t =
  | Debugger_statement
  | Multiple_values
  | Empty_consequence
  | Asynchronicity
  | Range_over_keys
  | Invalid_loop_index
  | Not_a_variable
  | Var_declaration
  | Do_while_loop
  | Exception
  | With_statement
  | Label
  | Continue
  | Type_assertion
  | Type_predicate
  | Abstract_class
  | Module
  | Type_constraint
  | Default_type_parameter
  | Enumerated
  | Ambient_declaration
  | Any_type
  | Number_type
  | Symbol_type
  | Unique_symbol_type
  | Void_type
  | Unknown_type
  | Never_type
  | Object_type
  | Array_type
  | Unsupported_tuple_member
  | Maybe_type
  | Type_query
  | Index_type_query
  | This_type
  | Existential_type
  | Unsupported_number
  | Non_integer_as_type
  | Unary_type
  | Singleton_type_true
  | Singleton_type_false
  | Null_type
  | Undefined_type
  | Lookup_type
  | Conditional_type
  | Template_literal_type
  | Intersection_type
  | Readonly_type
  | Constructor_type
  | Type_parameter_instantiation
  | Class_instantiation
  | Type_check
  | Generator
  | Undefined_value
  | Missing_type
  | Optional_parameter
  | Default_argument
  | Decorated_parameter
  | Access_parameter
  | Override_parameter
  | Readonly_parameter
  | Non_variable_parameter
  | Constant_type
  | Non_null_pattern
  | Multiple_defaults
  | Namespace_string
  | Namespace_nested

type error = t

(* Making errors *)

val make : ?hint:string -> Region.t -> error -> (_, string) Result.t

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
  | This
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
  | Non_null
  | Multiple_defaults
  | Namespace_string
  | Namespace_nested
  | Definite_asgmt_assertion
  | Unitialised_variable
  | Namespace_expression
  | Top_rest_pattern
  | Asgmt_pattern_in_array
  | Complex_rest_pattern
  | Subscript_pattern
  | Member_pattern
  | Bit_usr_eq
  | Exp_eq
  | Log_and_eq
  | Log_or_eq
  | Complex_lhs
  | Unary_add
  | Typeof_void_delete
  | Class_expression
  | Metaproperty
  | Null_value
  | Binary_octal
  | Non_integer
  | Regex
  | Super
  | Template_string
  | Return_type_absent
  | Named_lambda
  | Private_property
  | Strict_equality
  | Instanceof
  | In
  | Import
  | Spread_expression
  | Optional_chaining
  | Type_parameters_on_args
  | Invalid_subscript
  | Finalised_const
  | Assignment_in_pattern
  | Rest_pattern_in_lhs
  | Object_pattern_in_lhs
  | Rest_in_object_pattern
  | Asgmt_in_object_pattern
  | Property_as_string
  | Property_as_number
  | Computed_property_name
  | Invalid_parameter_of
  | Invalid_contract_of
  | Export_member
  | Constructor
  | Index_signature
  | Call_signature
  | Property_access
  | Property_scope
  | Set_get_all
  | Extends_clause
  | Member_decorator
  | Type_arguments_in_decorator
  | Multiple_arguments_in_decorator
  | Invalid_decorator_argument
  | Abstract_method
  | Call_static_block
  | Method_signature_in_class
  | Public_field_scope
  | Declare_definition
  | Field_mode

type error = t

(* Making errors *)

val make : ?hint:string -> Region.t -> error -> (_, string) Result.t

(* This module defines the syntax errors when printing the CST *)

(* Errors *)

type t =
  | Expression
  | Export_clause
  | Export
  | Export_clause_or_all
  | Named_imports_or_all_or_id
  | Named_imports_or_all
  | Type_or_conjunction
  | Type_or_disjunction
  | File_path
  | Identifier_or_string
  | String
  | Identifier
  | Parenthesized_expression
  | Statement
  | Switch_body
  | Initial_assignment
  | Expression_or_semicolon
  | In_or_of
  | Block
  | Label
  | Function_name
  | Parameters
  | Class_name
  | Class_body
  | Let_or_const
  | Variable
  | Type_annotation
  | Module_name
  | Namespace_name
  | Type_name
  | Type
  | Type_parameter
  | Enumeration_name
  | Enumeration
  | Interface_name
  | Interface_body
  | Augmented_assignment
  | Unary_operator
  | Binary_operator
  | Type_arguments
  | Property_identifier
  | Property_name
  | Block_or_expression
  | Arguments
  | Member_or_call
  | Member_expression
  | Identifier_or_member
  | Type_identifier_or_path
  | Identifier_or_rest
  | Object_denotation
  | Type_or_string_or_number
  | Function_denotation
  | Identifier_or_type
  | Pattern
  | Identifier_or_path

type error = t

val to_string : error -> string

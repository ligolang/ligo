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

let to_string : t -> string = function
  | Expression -> "An expression is expected."
  | Export_clause -> "An export clause is expected."
  | Export -> "The keyword 'export' is expected."
  | Export_clause_or_all -> "An export clause or '*' is expected."
  | Named_imports_or_all_or_id -> "Named imports or '*' or identifier are expected."
  | Named_imports_or_all -> "Named imports or '*' are expected."
  | Type_or_conjunction -> "A type or '&' is expected."
  | Type_or_disjunction -> "A type or '|' is expected."
  | File_path -> "A file path in a string is expected."
  | Identifier_or_string -> "An identifier or string is expected."
  | String -> "A string is expected."
  | Identifier -> "An identifier is expected."
  | Parenthesized_expression -> "A parenthesized expression is expected."
  | Statement -> "A statement is expected."
  | Switch_body -> "The body of the switch is expected."
  | Initial_assignment -> "An initial value assingment is expected."
  | Expression_or_semicolon -> "An expression or a semicolon is expected."
  | In_or_of -> "The keyword 'in' or 'of' is expected."
  | Block -> "A block of statements is expected."
  | Label -> "A label is expected."
  | Function_name -> "A function name is expected."
  | Parameters -> "Parameters are expected."
  | Class_name -> "A class name is expected."
  | Class_body -> "A class body is expected."
  | Let_or_const -> "The keyword 'let' or 'const' is expected."
  | Variable -> "A variable is expected."
  | Type_annotation -> "A type annotation is expected."
  | Module_name -> "A module name is expected."
  | Namespace_name -> "A namespace name is expected."
  | Type_name -> "A type name is expected."
  | Type -> "A type is expected."
  | Type_parameter -> "A type parameter is expected."
  | Enumeration_name -> "An enumeration name is expected."
  | Enumeration -> "An enumeration is expected."
  | Interface_name -> "An interface name is expected."
  | Interface_body -> "An interface body is expected."
  | Augmented_assignment -> "An augmented assignment operator is expected."
  | Unary_operator -> "A unary operator is expected."
  | Binary_operator -> "A binary operator is expected."
  | Type_arguments -> "Type arguments are expected."
  | Property_identifier -> "A property identifier is expected."
  | Property_name -> "A property name is expected."
  | Block_or_expression -> "A block of statements or an expression is expected."
  | Arguments -> "Arguments are expected."
  | Member_or_call -> "A member expression or call is expected."
  | Member_expression -> "A member expression is expected."
  | Identifier_or_member -> "An identifier or a member expression is expected."
  | Type_identifier_or_path -> "A type identifier, perhaps qualified, is expected."
  | Identifier_or_rest -> "An identifier or a rest pattern is expected."
  | Object_denotation -> "An object denotation is expected."
  | Type_or_string_or_number -> "A type, string or number is expected."
  | Function_denotation -> "A function denotation is expected."
  | Identifier_or_type -> "An identifier or a predefined type is expected."
  | Pattern -> "A pattern is expected."
  | Identifier_or_path -> "An identifier, perhaps qualified, is expected."

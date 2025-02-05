(* This module defines the syntax errors when printing the CST *)

(* Errors *)

type t =
  (* Keywords *)
  | Infer
  | Keyof
  | Meta
  | Target
  | False
  | True
  | Super
  | Null
  | Satisfies
  | Yield
  | New
  | Instanceof
  | Implements
  | Assert
  | As
  | Async
  | Function
  | Override
  | Readonly
  | Public
  | Private
  | Protected
  | Set
  | Get
  | All
  | Static
  | This
  | Is
  | Class
  | Const
  | Constraint
  | Let
  | Undefined
  | Abstract
  | Declare
  | Accessor
  | Global
  | Module
  | Enum
  | Import
  | Interface
  | Extends
  | Namespace
  | Type
  | Using
  | Return
  | Switch
  | Case
  | Default
  | Throw
  | While
  | With
  | Any
  | Number
  | Boolean
  | String
  | Symbol
  | Unique_symbol
  | Void
  | Unknown
  | Never
  | Object
  | Asserts
  | Debugger
  | Break
  | Continue
  | Do
  | Export
  | For
  | From
  | Await
  | Var
  | In
  | Of
  | If
  | Else
  | Typeof
  | Try
  | Catch
  | Require
  | Delete
  | Finally
  (* Symbols *)
  | Left_brace
  | Right_brace
  | Left_chevron
  | Right_chevron
  | Left_bracket
  | Right_bracket
  | Left_parenthesis
  | Right_parenthesis
  | Asterisk
  | Equal
  | Question_mark
  | Plus_equal
  | Minus_equal
  | Mult_equal
  | Div_equal
  | Rem_equal
  | Xor_equal
  | And_equal
  | Or_equal
  | Right_shift_equal
  | Unsigned_right_shift_equal
  | Left_shift_equal
  | Unsigned_left_shift_equal
  | Exponent_equal
  | Conjunction_equal
  | Disjunction_equal
  | Non_null_equal
  | Exclamation_mark
  | Tilde
  | Minus
  | Plus
  | Conjunction
  | Disjunction
  | Right_shift
  | Unsigned_right_shift
  | Left_shift
  | Unsigned_left_shift
  | And
  | Xor
  | Or
  | Div
  | Rem
  | Exponent
  | Lower_than
  | Lower_than_or_equal
  | No_conv_equal
  | Different
  | No_conv_different
  | Greater_than_or_equal
  | Greater_than
  | Non_null
  (* Syntax errors *)
  | Expression
  | Export_clause
  | Export_clause_or_all
  | Named_imports_or_all_or_id
  | Named_imports_or_all
  | Type_or_conjunction
  | Type_or_disjunction
  | File_path
  | Identifier_or_string
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
  | Type_expression
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
  | Type_or_typeof
  | New_or_import
  | Target_or_meta
  | String_literal
  | Regexp
  | Number_literal
  | Namespace_export
  | Import_clause
  | Namespace_or_named_imports

type error = t

(* -> "The keyword '' is expected." *)

let to_string : t -> string = function
  | Infer -> "The keyword 'infer' is expected."
  | Keyof -> "The keyword 'keyof' is expected."
  | Meta -> "The keyword 'meta' is expected."
  | Target -> "The keyword 'target' is expected."
  | False -> "The keyword 'false' is expected."
  | True -> "The keyword 'true' is expected."
  | Super -> "The keyword 'super' is expected."
  | Null -> "The keyword 'null' is expected."
  | Satisfies -> "The keyword 'satisfies' is expected."
  | Yield -> "The keyword 'yield' is expected."
  | New -> "The keyword 'new' is expected."
  | Instanceof -> "The keyword 'instanceof' is expected."
  | Implements -> "The keyword 'implements' is expected."
  | Assert -> "The keyword 'assert' is expected."
  | As -> "The keyword 'as' is expected."
  | Async -> "The keyword 'async' is expected."
  | Function -> "The keyword 'function' is expected."
  | Override -> "The keyword 'override' is expected."
  | Readonly -> "The keyword 'readonly' is expected."
  | Public -> "The keyword 'public' is expected."
  | Private -> "The keyword 'private' is expected."
  | Protected -> "The keyword 'protected' is expected."
  | Set -> "The keyword 'set' is expected."
  | Get -> "The keyword 'get' is expected."
  | All -> "The keyword 'all' is expected."
  | Static -> "The keyword 'static' is expected."
  | This -> "The keyword 'this' is expected."
  | Is -> "The keyword 'is' is expected."
  | Class -> "The keyword 'class' is expected."
  | Const -> "The keyword 'const' is expected."
  | Constraint -> "The keyword 'constraint' is expected."
  | Let -> "The keyword 'let' is expected."
  | Undefined -> "The keyword 'undefined' is expected."
  | Abstract -> "The keyword 'abstract' is expected."
  | Declare -> "The keyword 'declare' is expected."
  | Accessor -> "The keyword 'accessor' is expected."
  | Global -> "The keyword 'global' is expected."
  | Module -> "The keyword 'module' is expected."
  | Enum -> "The keyword 'enum' is expected."
  | Import -> "The keyword 'import' is expected."
  | Interface -> "The keyword 'interface' is expected."
  | Extends -> "The keyword 'extends' is expected."
  | Namespace -> "The keyword 'namespace' is expected."
  | Type -> "The keyword 'type' is expected."
  | Using -> "The keyword 'using' is expected."
  | Return -> "The keyword 'return' is expected."
  | Switch -> "The keyword 'switch' is expected."
  | Case -> "The keyword 'case' is expected."
  | Default -> "The keyword 'default' is expected."
  | Throw -> "The keyword 'throw' is expected."
  | While -> "The keyword 'while' is expected."
  | With -> "The keyword 'with' is expected."
  | Any -> "The keyword 'any' is expected."
  | Number -> "The keyword 'number' is expected."
  | Boolean -> "The keyword 'boolean' is expected."
  | String -> "The keyword 'string' is expected."
  | Symbol -> "The keyword 'symbol' is expected."
  | Unique_symbol -> "The keywords 'unique symbol' are expected."
  | Void -> "The keyword 'void' is expected."
  | Unknown -> "The keyword 'unknown' is expected."
  | Never -> "The keyword 'never' is expected."
  | Object -> "The keyword 'object' is expected."
  | Asserts -> "The keyword 'asserts' is expected."
  | Debugger -> "The keyword 'debugger' is expected."
  | Break -> "The keyword 'break' is expected."
  | Continue -> "The keyword 'continue' is expected."
  | Do -> "The keyword 'do' is expected."
  | Export -> "The keyword 'export' is expected."
  | For -> "The keyword 'for' is expected."
  | From -> "The keyword 'from' is expected."
  | Await -> "The keyword 'await' is expected."
  | Var -> "The keyword 'var' is expected."
  | In -> "The keyword 'in' is expected."
  | Of -> "The keyword 'of' is expected."
  | If -> "The keyword 'if' is expected."
  | Else -> "The keyword 'else' is expected."
  | Typeof -> "The keyword 'typeof' is expected."
  | Try -> "The keyword 'try' is expected."
  | Catch -> "The keyword 'catch' is expected."
  | Require -> "The keyword 'require' is expected."
  | Delete -> "The keyword 'delete' is expected."
  | Finally -> "The keyword 'finally' is expected."
  (* Symbols *)
  | Left_brace -> "The symbol '{' is expected."
  | Right_brace -> "The symbol '}' is expected."
  | Left_chevron -> "The symbol '<' is expected."
  | Right_chevron -> "The symbol '>' is expected."
  | Left_bracket -> "The symbol '[' is expected."
  | Right_bracket -> "The symbol ']' is expected."
  | Left_parenthesis -> "The symbol '(' is expected."
  | Right_parenthesis -> "The symbol ')' is expected."
  | Asterisk -> "The symbol '*' is expected."
  | Equal -> "The symbol '=' is expected."
  | Question_mark -> "The symbol '?' is expected."
  | Plus_equal -> "The symbol '+=' is expected."
  | Minus_equal -> "The symbol '-=' is expected."
  | Mult_equal -> "The symbol '*=' is expected."
  | Div_equal -> "The symbol '/=' is expected."
  | Rem_equal -> "The symbol '%=' is expected."
  | Xor_equal -> "The symbol '^=' is expected."
  | And_equal -> "The symbol '&=' is expected."
  | Or_equal -> "The symbol '|=' is expected."
  | Right_shift_equal -> "The symbol '>>=' is expected."
  | Unsigned_right_shift_equal -> "The symbol '>>>=' is expected."
  | Left_shift_equal -> "The symbol '<<=' is expected."
  | Unsigned_left_shift_equal -> "The symbol '<<<=' is expected."
  | Exponent_equal -> "The symbol '**=' is expected."
  | Conjunction_equal -> "The symbol '&&=' is expected."
  | Disjunction_equal -> "The symbol '||=' is expected."
  | Non_null_equal -> "The symbol '??=' is expected."
  | Exclamation_mark -> "The symbol '!' is expected."
  | Tilde -> "The symbol '~' is expected."
  | Minus -> "The symbol '-' is expected."
  | Plus -> "The symbol '+' is expected."
  | Conjunction -> "The symbol '&&' is expected."
  | Disjunction -> "The symbol '||' is expected."
  | Right_shift -> "The symbol '>>' is expected."
  | Unsigned_right_shift -> "The symbol '>>>' is expected."
  | Left_shift -> "The symbol '<<' is expected."
  | Unsigned_left_shift -> "The symbol '<<<' is expected."
  | And -> "The symbol '&' is expected."
  | Xor -> "The symbol '^' is expected."
  | Or -> "The symbol '|' is expected."
  | Div -> "The symbol '/' is expected."
  | Rem -> "The symbol '%' is expected."
  | Exponent -> "The symbol '**' is expected."
  | Lower_than -> "The symbol '<' is expected."
  | Lower_than_or_equal -> "The symbol '<=' is expected."
  | No_conv_equal -> "The symbol '===' is expected."
  | Different -> "The symbol '!=' is expected."
  | No_conv_different -> "The symbol '!==' is expected."
  | Greater_than_or_equal -> "The symbol '>=' is expected."
  | Greater_than -> "The symbol '>' is expected."
  | Non_null -> "The symbol '??' is expected."

  (* Syntax errors *)
  | Expression -> "An expression is expected."
  | Export_clause -> "An export clause is expected."
  | Export_clause_or_all -> "An export clause or '*' is expected."
  | Named_imports_or_all_or_id -> "Named imports or '*' or identifier are expected."
  | Named_imports_or_all -> "Named imports or '*' are expected."
  | Type_or_conjunction -> "A type or '&' is expected."
  | Type_or_disjunction -> "A type or '|' is expected."
  | File_path -> "A file path in a string is expected."
  | Identifier_or_string -> "An identifier or string is expected."
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
  | Type_expression -> "A type is expected."
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
  | Type_or_typeof -> "The keyword 'type' or 'typeof' is expected."
  | New_or_import -> "The keyword 'new' or 'import' is expected."
  | Target_or_meta -> "The keyword 'target' or 'meta' is expected."
  | String_literal -> "A string is expected."
  | Regexp -> "A regular expression is expected."
  | Number_literal -> "A number is expected."
  | Namespace_export -> "A namespace export clause is expected."
  | Import_clause -> "An import clause is expected."
  | Namespace_or_named_imports -> "A namespace import clause or named imports are expected."

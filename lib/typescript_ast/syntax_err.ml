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
  | Strict_equal
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
  | Unsigned_shift_right_equal
  | Left_shift_equal
  | Unsigned_shift_left_equal
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
  | Unsigned_shift_right
  | Left_shift
  | Unsigned_shift_left
  | And
  | Xor
  | Or
  | Div
  | Rem
  | Exponent
  | Less_than
  | Less_than_or_equal
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
  | Module_declaration
  | Namespace_name
  | Namespace_declaration
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
  | Namespace_or_named_imports_or_ident
  | Namespace_import
  | Named_imports
  | Import_specifier
  | Import_require_clause
  | Import_attribute
  | For_or_await
  | Empty_statement
  | Declaration
  | Function_declaration
  | Generator_function_declaration
  | Class_declaration
  | Async_or_function
  | Const_or_type_name
  | Const_or_enum
  | Glimmer_template
  | Using_or_expression
  | LHS_of_augmented_assgmnt
  | Template_string
  | Object_expression
  | Key_value_pair
  | Array
  | Type_parameters
  | Spread
  | Function_expression
  | Arrow_function
  | Generator_function
  | Class_expression
  | Extends_or_implements
  | Implements_clause
  | Extends_clause
  | Method_definition
  | Static_block
  | Abstract_method_signature
  | Public_field_definition
  | Meta_property
  | Call_expression
  | Non_null_expression
  | Parenthesized_type
  | Predefined_type
  | Nested_type_identifier
  | Generic_type
  | Object_type
  | Property_signature
  | Call_signature
  | Asserts_annotation
  | Type_predicate_annotation
  | Construct_signature
  | Index_signature
  | Mapped_type_signature
  | Omitting_type_annotation
  | Adding_type_annotation
  | Opting_type_annotation
  | Method_signature
  | Array_type
  | Tuple_type
  | Tuple_parameter
  | Optional_tuple_parameter
  | Rest_pattern
  | Optional_type
  | Rest_type
  | Type_query
  | Type_query_subscript
  | Type_query_member
  | Type_query_instantiation
  | Type_query_call
  | Index_type_query
  | Existential_type
  | Literal_type
  | Lookup_type
  | Conditional_type
  | Template_literal_type
  | Intersection_type
  | Union_type
  | Function_type
  | Type_predicate
  | Readonly_type
  | Constructor_type
  | Required_parameter
  | Decorator
  | Decorator_member
  | Decorator_call
  | Parenthesized_decorator
  | Public_private_protected
  | Object_pattern
  | Pair_pattern
  | Assignment_pattern
  | Computed_property_name
  | Object_assignment_pattern
  | Array_pattern
  | Let_or_const_or_var
  | Const_or_type
  | Optional_chain
  | Index_expression
  | Object_field
  | Array_cell
  | Class_member
  | Import_or_expression
  | Argument
  | Object_type_field
  | Asserted
  | Type_of_index_signature
  | Plus_or_minus
  | Parameter
  | Object_pattern_field
  | Value_of_pair_pattern
  | Object_or_array_pattern
  | Array_cell_pattern
  | Increment
  | Decrement
  | Backquote
  | Colon
  | Ellipsis
  | Arrow
  | Dot
  | Ampersand
  | Vertical_bar
  | Incr_or_decr_or_expr
  | Increment_or_decrement
  | Block_or_ident_or_decl
  | Type_alias_declaration
  | Variable_declaration
  | String_or_type
  | Selector_or_optional_chain

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
  | Strict_equal -> "The symbol '==' is expected."
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
  | Unsigned_shift_right_equal -> "The symbol '>>>=' is expected."
  | Left_shift_equal -> "The symbol '<<=' is expected."
  | Unsigned_shift_left_equal -> "The symbol '<<<=' is expected."
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
  | Unsigned_shift_right -> "The symbol '>>>' is expected."
  | Left_shift -> "The symbol '<<' is expected."
  | Unsigned_shift_left -> "The symbol '<<<' is expected."
  | And -> "The symbol '&' is expected."
  | Xor -> "The symbol '^' is expected."
  | Or -> "The symbol '|' is expected."
  | Div -> "The symbol '/' is expected."
  | Rem -> "The symbol '%' is expected."
  | Exponent -> "The symbol '**' is expected."
  | Less_than -> "The symbol '<' is expected."
  | Less_than_or_equal -> "The symbol '<=' is expected."
  | No_conv_equal -> "The symbol '===' is expected."
  | Different -> "The symbol '!=' is expected."
  | No_conv_different -> "The symbol '!==' is expected."
  | Greater_than_or_equal -> "The symbol '>=' is expected."
  | Greater_than -> "The symbol '>' is expected."
  | Non_null -> "The symbol '??' is expected."
  | Increment -> "The symbol '++' is expected."
  | Decrement -> "The symbol '--' is expected."
  | Backquote -> "A backquote '`' is expected."
  | Colon -> "A colon ':' is expected."
  | Ellipsis -> "The symbol '...' is expected."
  | Arrow -> "The symbol '=>' is expected."
  | Dot -> "The symbol '.' is expected."
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
  | Module_declaration -> "A module declaration is expected."
  | Namespace_name -> "A namespace name is expected."
  | Namespace_declaration -> "A namespace declaration is expected."
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
  | Namespace_or_named_imports ->
    "A namespace import clause or named imports are expected."
  | Namespace_or_named_imports_or_ident ->
    "A namespace import clause or named imports or an identifier are expected."
  | Namespace_import -> "A namespace import ('* as') is expected."
  | Named_imports -> "Named imports (between braces) are expected."
  | Import_specifier ->
    "An import specifier ('type', 'typeof', identifier, string) is expected."
  | Import_require_clause -> "An import requirement is expected."
  | Import_attribute -> "The keyword 'with' or 'assert' is expected."
  | For_or_await -> "The keyword 'for' or 'await' is expected."
  | Empty_statement -> "An empty statement was expected."
  | Declaration -> "A declaration is expected."
  | Function_declaration -> "A function declaration is expected."
  | Generator_function_declaration -> "A generator function declaration is expected."
  | Class_declaration -> "A class declaration is expected."
  | Async_or_function -> "The keyword 'async' or 'function' is expected."
  | Const_or_type_name -> "A type name or the keyword 'const' is expected."
  | Const_or_enum -> "The keyword 'const' or 'enum' is expected."
  | Glimmer_template -> "A glimmer template is expected."
  | Using_or_expression -> "The keyword 'using' or an expression is expected."
  | LHS_of_augmented_assgmnt -> "The left-hand side of an assignment is expected."
  | Template_string -> "A template string is expected."
  | Object_expression -> "A object is expected."
  | Key_value_pair -> "A key-value pair is expected."
  | Array -> "An array is expected."
  | Type_parameters -> "Type parameters are expected."
  | Spread -> "A spread element is expected."
  | Function_expression -> "A function expression is expected."
  | Arrow_function -> "An arrow function is expected."
  | Generator_function -> "A generator function is expected."
  | Class_expression -> "A class is expected."
  | Extends_or_implements -> "A class 'extends' or 'implements' clause is expected."
  | Implements_clause -> "An 'implements' clause is expected."
  | Extends_clause -> "An 'extends' clause is expected."
  | Method_definition -> "A method definition is expected."
  | Static_block -> "A static block is expected."
  | Abstract_method_signature -> "An abstract method signature is expected."
  | Public_field_definition -> "A public field definition is expected."
  | Meta_property -> "A meta-property is expected."
  | Call_expression -> "A call expression is expected."
  | Non_null_expression -> "A non-null expression is expected."
  | Parenthesized_type -> "A parenthesized type is expected."
  | Predefined_type -> "A predefined type is expected."
  | Nested_type_identifier -> "A type name, perhaps qualified, is expected."
  | Generic_type -> "A generic type is expected."
  | Object_type -> "An object type is expected."
  | Property_signature -> "A property signature is expected."
  | Call_signature -> "A call signature is expected."
  | Asserts_annotation -> "An 'asserts' annotation is expected."
  | Type_predicate_annotation -> "A type predicate annotation is expected."
  | Construct_signature -> "A construct signature is expected."
  | Index_signature -> "An index signature is expected."
  | Mapped_type_signature ->
    "A mapped type signature is expected in this index signature."
  | Omitting_type_annotation -> "A type omission annotation '-?:' is expected."
  | Adding_type_annotation -> "A type addition annotation '+?:' is expected."
  | Opting_type_annotation -> "An optional type annotation '?:' is expected."
  | Ampersand -> "The symbol '&' is expected."
  | Vertical_bar -> "The symbol '|' is expected."
  | Method_signature -> "A method signature is expected."
  | Array_type -> "An array type is expected."
  | Tuple_type -> "A tuple type is expected."
  | Tuple_parameter -> "A tuple parameter is expected."
  | Optional_tuple_parameter -> "Optional tuple parameter expected."
  | Rest_pattern -> "A rest pattern ('...') is expected."
  | Optional_type -> "An optional type ('?') is expected."
  | Rest_type -> "A rest type ('...') is expected."
  | Type_query -> "A type query ('typeof') is expected."
  | Type_query_subscript -> "A subscript expression is expected in a type query."
  | Type_query_member -> "A member expression is expected in a type query."
  | Type_query_instantiation -> "An instantiation is expected in a type query."
  | Type_query_call -> "A function call is expected in a type query."
  | Index_type_query -> "An index type query is expected."
  | Existential_type -> "An existential type ('*') is expected."
  | Literal_type -> "A literal type is expected."
  | Lookup_type -> "A lookup type is expected."
  | Conditional_type -> "A conditional type is expected."
  | Template_literal_type -> "A template literal type is expected."
  | Intersection_type -> "An intersection type is expected."
  | Union_type -> "A union type is expected."
  | Function_type -> "A function type is expected."
  | Type_predicate -> "A type predicate is expected."
  | Readonly_type -> "A readonly type is expected."
  | Constructor_type -> "A constructor type is expected."
  | Required_parameter -> "A required parameter is expected."
  | Decorator -> "A decorator is expected."
  | Decorator_member -> "A member expression is expected as a decorator."
  | Decorator_call -> "A decorator with an argument is expected."
  | Parenthesized_decorator -> "A parenthesized decorator is expected."
  | Public_private_protected ->
    "The keyword 'public', private, or 'protected' is expected."
  | Object_pattern -> "An object pattern is expected."
  | Pair_pattern -> "A pattern for a key-value pair is expected."
  | Assignment_pattern -> "An assignment pattern is expected."
  | Computed_property_name -> "A property name is expected between brackets."
  | Object_assignment_pattern -> "A assignment pattern for an object is expected."
  | Array_pattern -> "An array pattern is expected."
  | Let_or_const_or_var -> "The keyword 'let' or 'const' or 'var' is expected."
  | Const_or_type -> "The keyword 'const' or a type is expected."
  | Optional_chain -> "The optional chain symbol '?.' is expected."
  | Index_expression -> "An index expression is expected."
  | Object_field -> "An object field is expected."
  | Array_cell -> "An array cell is expected."
  | Class_member -> "A class member is expected."
  | Import_or_expression -> "The keyword 'import' or an expression is expected."
  | Argument -> "An argument is expected."
  | Object_type_field -> "A field for the object type is expected."
  | Asserted -> "A type predicate, or identifier or keyword `this` is expected."
  | Type_of_index_signature -> "A type annotation for the index signature is expected."
  | Plus_or_minus -> "The operator '+' or '-' is expected."
  | Parameter -> "A parameter is expected."
  | Object_pattern_field -> "A field in the object pattern is expected."
  | Value_of_pair_pattern -> "A pattern for the value of a key-value pair is expected."
  | Object_or_array_pattern -> "An object or array pattern is expected."
  | Array_cell_pattern -> "A pattern for an array element is expected."
  | Incr_or_decr_or_expr -> "An operator '--' or '++' or an expression is expected."
  | Increment_or_decrement -> "An operator '--' or '++' is expected."
  | Block_or_ident_or_decl ->
    "A block or a property identifier or a declaration is expected."
  | Type_alias_declaration -> "A type alias declaration is expected."
  | Variable_declaration -> "A variable declaration is expected."
  | String_or_type -> "A string literal or a type expression is expected."
  | Selector_or_optional_chain -> "A selector '.' or an optional chain '?.' is expected."

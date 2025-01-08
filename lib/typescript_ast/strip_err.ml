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
  | Interface_with_type_parameters
  | Generic_class_extension

type error = t

let to_string = function
  | Debugger_statement -> "Debugger statements are not supported in JsLIGO."
  | Multiple_values -> "Multiple values are not supported in JsLIGO."
  | Empty_consequence -> "Empty consequences are not supported in JsLIGO."
  | Asynchronicity -> "Asynchronicity is not supported in JsLIGO."
  | Range_over_keys -> "Ranging over keys only is not supported in JsLIGO"
  | Invalid_loop_index ->
    "Only a variable or a pair key-value (for maps) can index loops in JsLIGO."
  | Not_a_variable -> "Expected a variable."
  | Var_declaration -> "'var' declarations are not supported in JsLIGO."
  | Do_while_loop -> "'do-while' loops are not supported in JsLIGO."
  | Exception -> "Exceptions are not supported in JsLIGO."
  | With_statement -> "'with' statements are not supported in JsLIGO."
  | Label -> "Branching on labels is not supported in JsLIGO."
  | Continue -> "'continue' statements are not supported in JsLIGO."
  | Type_assertion -> "Type assertions are not supported in JsLIGO."
  | Type_predicate -> "Type predicates are not supported in JsLIGO."
  | Abstract_class -> "Abstract classes are not supported in JsLIGO."
  | Module -> "Modules are not supported in JsLIGO."
  | Type_constraint -> "Type constraints are not supported in JsLIGO."
  | Default_type_parameter -> "Default type parameters are not supported in JsLIGO."
  | Enumerated -> "Enumerated values are not supported in JsLIGO."
  | Ambient_declaration -> "Ambient declarations are not supported in JsLIGO."
  | Any_type -> "The type 'any' is not supported in JsLIGO."
  | Number_type -> "The type 'number' is not supported in JsLIGO."
  | Symbol_type -> "The type 'symbol' is not supported in JsLIGO."
  | Unique_symbol_type -> "The type 'unique symbol' is not supported in JsLIGO."
  | Void_type -> "The type type 'void' is not supported by JsLIGO."
  | Unknown_type -> "The type 'unknown' is not supported by JsLIGO."
  | Never_type -> "Type type 'never' is not supported by JsLIGO."
  | Object_type -> "The type 'object' is not supported by JsLIGO."
  | Array_type -> "The type 'array' is not supported by JsLIGO."
  | Unsupported_tuple_member -> "Unsupported tuple member in JsLIGO."
  | Maybe_type -> "Maybe types are not supported in JsLIGO."
  | Type_query -> "Type queries are not supported in JsLIGO."
  | Index_type_query -> "Index type queries are not supported in JsLIGO."
  | This -> "Keyword 'this' is not supported in JsLIGO."
  | Existential_type -> "Existential types are not supported in JsLIGO."
  | Unsupported_number -> "This number literal is not supported in JsLIGO."
  | Non_integer_as_type -> "Non-integer numbers as types are not supported by JsLIGO."
  | Unary_type -> "Unary type are not supported in JsLIGO."
  | Singleton_type_true -> "The singleton type 'true' is not supported by JsLIGO."
  | Singleton_type_false -> "The singleton type 'false' is not supported by JsLIGO."
  | Null_type -> "The type 'null' is not supported by JsLIGO."
  | Undefined_type -> "The type 'undefined' is not supported by JsLIGO."
  | Lookup_type -> "Lookup types are not supported in JsLIGO."
  | Conditional_type -> "Conditional types are not supported in JsLIGO."
  | Template_literal_type -> "Template literal type are not supported in JsLIGO."
  | Intersection_type -> "Intersection types are not supported in JsLIGO."
  | Readonly_type -> "Read-only types are not supported in JsLIGO."
  | Constructor_type -> "Constructor types are not supported in JsLIGO."
  | Type_parameter_instantiation ->
    "Instantiation of type parameters is not supported in JsLIGO."
  | Class_instantiation -> "Instantiation of classes is not supported in JsLIGO."
  | Type_check -> "Type checks are not supported in JsLIGO."
  | Generator -> "Generators are not supported in JsLIGO."
  | Undefined_value -> "Undefined values are not supported in patterns in JsLIGO."
  | Missing_type -> "Type annotations in function/property types are mandatory in JsLIGO."
  | Optional_parameter -> "Optional parameters are not supported in JsLIGO."
  | Default_argument -> "Default parameter values are not supported in JsLIGO."
  | Decorated_parameter ->
    "Decorators on function parameters are not supported in JsLIGO."
  | Access_parameter ->
    "Accessibility modifiers on function parameters are not supported in JsLIGO."
  | Override_parameter ->
    "Override modifier on function parameters not supported in JsLIGO."
  | Readonly_parameter ->
    "Read-only modifier on function parameters not supported in JsLIGO."
  | Non_variable_parameter ->
    "Only variables are supported as function parameters in JsLIGO."
  | Constant_type -> "Constant types are not supported in JsLIGO."
  | Non_null -> "Non-null values are not supported in JsLIGO."
  | Multiple_defaults -> "Multiple default switch cases are not supported in JsLIGO."
  | Namespace_string -> "Namespace names as strings are not supported in JsLIGO."
  | Namespace_nested -> "Nested namespace names are not supported in JsLIGO."
  | Definite_asgmt_assertion ->
    "Definite assignment assertions are not supported in JsLIGO."
  | Unitialised_variable -> "Unitialised variables are not supported in JsLIGO."
  | Namespace_expression -> "Namespace expressions are not supported in JsLIGO."
  | Top_rest_pattern ->
    "Rest patterns are only supported in the arrays/tuples/lists of JsLIGO."
  | Asgmt_pattern_in_array -> "Assignments in array patterns are not supported in JsLIGO."
  | Complex_rest_pattern -> "Complex rest patterns are not supported in JsLIGO."
  | Subscript_pattern -> "Subscript patterns are not supported in JsLIGO."
  | Member_pattern -> "Member patterns are not supported in JsLIGO."
  | Bit_usr_eq -> "The augmented unsigned bitwise shift-right is not supported in JsLIGO."
  | Exp_eq -> "The augmented exponent is not supported in JsLIGO."
  | Log_and_eq -> "The augmented logical conjunction is not supported in JsLIGO."
  | Log_or_eq -> "The augmented logical disjunction is not supported in JsLIGO."
  | Complex_lhs -> "Complex left-hand sides are not supported in JsLIGO."
  | Unary_add -> "Unary plus is not supported in JsLIGO."
  | Typeof_void_delete ->
    "Only arithmetic and logical unary operators are supported in JsLIGO."
  | Class_expression -> "Class expressions are not supported in JsLIGO."
  | Metaproperty -> "Meta-properties are not supported in JsLIGO."
  | Null_value -> "The null value is not supported in JsLIGO."
  | Binary_octal -> "Binary and octal numbers are not supported in JsLIGO."
  | Non_integer -> "Non-integer numbers are not supported in JsLIGO."
  | Regex -> "Regular expressions are not supported in JsLIGO."
  | Super -> "Superclasses are not supported in JsLIGO."
  | Template_string -> "Template strings are not supported in JsLIGO."
  | Return_type_absent ->
    "Function signatures without return types are not supported in JsLIGO."
  | Named_lambda -> "Named lambdas are not supported in JsLIGO."
  | Private_property -> "Private property names are not supported in JsLIGO."
  | Strict_equality -> "Strict equality is not supported in JsLIGO."
  | Instanceof -> "The operator 'instanceof' is not supported in JsLIGO."
  | In -> "The operator 'in' is not supported in JsLIGO."
  | Import -> "Keyword 'import' is not supported here in JsLIGO."
  | Spread_expression -> "Spread expressions are not supported here in JsLIGO."
  | Optional_chaining -> "Optional chaining is not supported in JsLIGO."
  | Type_parameters_on_args -> "Type parameters on arguments are not supported in JsLIGO."
  | Invalid_subscript -> "Only number literals as indices are supported in JsLIGO."
  | Finalised_const -> "Finalised (\"using\") constants are not supported in JsLIGO."
  | Assignment_in_pattern -> "Assignments in patterns are not supported in vJsLIGO."
  | Rest_pattern_in_lhs ->
    "Rest patterns in left-hand sides of assignments are not supported in JsLIGO."
  | Object_pattern_in_lhs ->
    "Object patterns in left-hand sides of assignments are not supported in JsLIGO."
  | Rest_in_object_pattern -> "Rest in object patterns are not supported in JsLIGO."
  | Asgmt_in_object_pattern ->
    "Assignments in object patterns are not supported in JsLIGO."
  | Property_as_string -> "Property names as strings are not supported in JsLIGO."
  | Property_as_number -> "Property names as numbers are not supported in JsLIGO."
  | Computed_property_name -> "Computed property names are not supported in JsLIGO."
  | Invalid_parameter_of -> "Multiple parameters of contract."
  | Invalid_contract_of -> "Multiple arguments for creating a contract."
  | Export_member -> "Exported members are not supported in JsLIGO."
  | Constructor -> "Constructors are not supported in JsLIGO."
  | Index_signature -> "Index signatures are not supported in JsLIGO."
  | Call_signature -> "Call signatures in object types are not supported in JsLIGO."
  | Property_access -> "Only public access is supported in JsLIGO."
  | Property_scope -> "Property scoping is not supported in JsLIGO."
  | Set_get_all -> "Setters and getters are not supported in JsLIGO."
  | Extends_clause -> "Extension of classes are not supported in JsLIGO."
  | Member_decorator -> "Decorators as member expressions are not supported in JsLIGO."
  | Type_arguments_in_decorator ->
    "Type arguments in decorators are not supported in JsLIGO."
  | Multiple_arguments_in_decorator ->
    "Multiple arguments in decorator are not supported in JsLIGO."
  | Invalid_decorator_argument ->
    "Only unqualified variables and strings can be decorators' argument in JsLIGO."
  | Abstract_method -> "Abstract methods are not supported in JsLIGO."
  | Call_static_block -> "Call static blocks are not supported in JsLIGO."
  | Method_signature_in_class ->
    "Method signatures in classes are not supported in JsLIGO."
  | Public_field_scope -> "The only public field scoping is \"static\" in JsLIGO."
  | Declare_definition -> "External definitions are not supported in JsLIGO."
  | Field_mode -> "Field modes are not suppored in JsLIGO."
  | Interface_with_type_parameters ->
    "Interfaces with type parameters are not supported in JsLIGO."
  | Generic_class_extension -> "Generic class extensions are not supported in JsLIGO."

(* Creating errors *)

let make ?(hint : string option) (region : Region.t) (error : t) =
  let hint =
    match hint with
    | None | Some "" -> ""
    | Some msg -> "\nHint: " ^ msg
  in
  Error (Printf.sprintf "%s:\n%s%s" (region#to_string `Byte) (to_string error) hint)

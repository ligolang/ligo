(* Abstract Syntax Tree (AST) for TypeScript

   We used the JavaScript tree-sitter grammar and the TypeScript
   tree-sitter grammar as reference. The excerpts from those grammars
   are in comment before the relevant AST nodes. *)

[@@@warning "-30"] (* Duplicate record field names *)

(* Utilities *)

(* Literals *)

type identifier = string
type template_string = string
type hash_name = string

type hex_literal = string * Hex.t
type dec_literal = string * Q.t
type bin_literal = string * Hex.t
type oct_literal = string * Hex.t

type bigint_literal =
  Hex_literal of hex_literal
| Bin_literal of bin_literal
| Oct_literal of oct_literal
| Dec_literal of dec_literal

(* Keywords *)

(* Symbols *)

(* The Abstract Syntax Tree *)

(* DECLARATIONS *)

(* JavaScript:

   declaration: $ => choice(
     $.function_declaration,
     $.generator_function_declaration,
     $.class_declaration,
     $.lexical_declaration,
     $.variable_declaration)

   TypeScript:

   declaration: ($, previous) => choice(
     previous,
     $.function_signature,
     $.abstract_class_declaration,
     $.module,
     prec('declaration', $.internal_module),
     $.type_alias_declaration,
     $.enum_declaration,
     $.interface_declaration,
     $.import_alias,
     $.ambient_declaration),

   module: $ => seq('module', $._module)
*)

type declaration =
  D_function_declaration of function_declaration
| D_generator_function_declaration of generator_function_declaration
| D_class_declaration of class_declaration
| D_lexical_declaration of lexical_declaration
| D_variable_declaration of variable_declaration
| D_function_signature of function_signature
| D_abstract_class_declaration of abstract_class_declaration
| D_module of module_
| D_internal_module of internal_module
| D_type_alias_declaration of type_alias_declaration
| D_enum_declaration of enum_declaration
| D_interface_declaration of interface_declaration
| D_import_alias of import_alias
| D_ambient_declaration of ambient_declaration

(* Function Declaration

   JavaScript:

   function_declaration: $ => prec.right('declaration', seq(
     optional('async'),
     'function',
     field('name', $.identifier),
     $._call_signature,
     field('body', $.statement_block),
     optional($._automatic_semicolon)))
*)

and function_declaration = {
  fun_sig: function_signature;
  body: statement_block
}

(* Function Signature

   TypeScript:

   function_signature: $ => seq(
     optional('async'),
     'function',
     field('name', $.identifier),
     $._call_signature,
     choice($._semicolon, $._function_signature_automatic_semicolon)),

   _call_signature: $ => seq(
     field('type_parameters', optional($.type_parameters)),
     field('parameters', $.formal_parameters),
     field('return_type', optional(
       choice($.type_annotation,
              $.asserts_annotation,
              $.type_predicate_annotation)))),

   formal_parameters: $ => seq(
     '(',
     optional(seq(
       commaSep1($._formal_parameter),
       optional(','))),
     ')'),

   _formal_parameter: $ => choice(
     $.required_parameter,
     $.optional_parameter),

   required_parameter: $ => seq(
     $._parameter_name,
     field('type', optional($.type_annotation)),
     optional($._initializer)),

   _initializer: $ => seq('=', field('value', $.expression)),

   _parameter_name: $ => seq(
     repeat(field('decorator', $.decorator)),
     optional($.accessibility_modifier),
     optional($.override_modifier),
     optional('readonly'),
     field('pattern', choice($.pattern, $.this))),

   accessibility_modifier: _ => choice(
     'public',
     'private',
     'protected'),

   override_modifier: _ => 'override',

   optional_parameter: $ => seq(
     $._parameter_name,
     '?',
     field('type', optional($.type_annotation)),
     optional($._initializer)),

   type_predicate_annotation: $ => seq(seq(':', $.type_predicate)), // Ouch

   type_predicate: $ => seq(
     field('name', choice(
       $.identifier,
       $.this,
       alias($.predefined_type, $.identifier))),
     'is',
     field('type', $.type))
*)

and function_signature = {
  async: bool;
  name: identifier;
  call_sig: call_signature;
}

and call_signature = {
  type_parameters: type_parameter list;
  parameters: formal_parameters;
  return_type: call_return_type option
}

and formal_parameters = formal_parameter list

and formal_parameter =
  Required_parameter of formal_parameter'
| Optional_parameter of formal_parameter'

and formal_parameter' = {
  parameter_name: parameter_name;
  type_: type_annotation option;
  default: expression option
}

and parameter_name = {
  decorator: decorator;
  access: accessibility_modifier option;
  override: override_modifier option;
  readonly: bool;
  pattern: parameter_pattern
}

and parameter_pattern =
  Parameter_pattern of pattern
| Parameter_this

and call_return_type =
  Type_annotation of type_annotation
| Asserts_annotation of asserts_annotation
| Type_predicate_annotation of type_predicate

and type_annotation = type_

and type_predicate = {
  name: type_predicate_name;
  type_: type_
}

and type_predicate_name =
  Type_predicate_identifier of identifier (* Including predefined types *)
| Type_predicate_this

(* Generator Function Declaration

   JavaScript:

   generator_function_declaration: $ => prec.right('declaration', seq(
     optional('async'),
     'function',
     '*',
     field('name', $.identifier),
     $._call_signature,
     field('body', $.statement_block),
     optional($._automatic_semicolon)))
*)

and generator_function_declaration =
  Generator_function_declaration of function_declaration

(* Class Declaration

   TypeScript:

   class_declaration: $ => prec.left('declaration', seq(
     repeat(field('decorator', $.decorator)),
     'class',
     field('name', $._type_identifier),
     field('type_parameters', optional($.type_parameters)),
     optional($.class_heritage),
     field('body', $.class_body),
     optional($._automatic_semicolon))),

   type_parameters: $ =>
     seq('<', commaSep1($.type_parameter), optional(','), '>'),

   type_parameter: $ => seq(
     optional('const'),
     field('name', $._type_identifier),
     field('constraint', optional($.constraint)),
     field('value', optional($.default_type))),

   constraint: $ => seq(choice('extends', ':'), $.type), // What is ":"?

   default_type: $ => seq('=', $.type),

   class_heritage: $ => choice(
     seq($.extends_clause, optional($.implements_clause)),
     $.implements_clause),

   extends_clause: $ => seq('extends', commaSep1($._extends_clause_single)),

   _extends_clause_single: $ => prec('extends', seq(
     field('value', $.expression),
     field('type_arguments', optional($.type_arguments)))),

   implements_clause: $ => seq('implements', commaSep1($.type)),

   type_arguments: $ => seq('<', commaSep1($.type), optional(','), '>'),

   class_body: $ => seq(
     '{',
     repeat(choice(
       seq(repeat(field('decorator', $.decorator)),
           $.method_definition,
           optional($._semicolon)),
       seq($.method_signature,
           choice($._function_signature_automatic_semicolon, ',')),
       $.class_static_block,
       seq(choice(
             $.abstract_method_signature,
             $.index_signature,
             $.method_signature,
             $.public_field_definition),
           choice($._semicolon, ',')),
       ';')),
     '}')

   JavaScript:

   class_static_block: $ => seq(
     'static',
     optional($._automatic_semicolon),
     field('body', $.statement_block))
*)

and class_declaration = {
  decorators: decorator list;
  name: type_identifier;
  type_parameters: type_parameter list;
  class_heritage: class_heritage option;
  body: class_member list
}

and class_heritage =
  Extends_clause of extends_clause * implements_clause option
| Implements_clause of implements_clause

and extends_clause = extends_clause_single Nonempty_list.t

and extends_clause_single = {
  value: expression;
  type_arguments: type_arguments option
}

and type_arguments = type_ Nonempty_list.t

and implements_clause = type_ Nonempty_list.t

and type_parameter = {
  const: bool;
  name: type_identifier;
  constraint_: type_ option;
  value: type_ option (* default *)
}

and class_body = class_member list

and class_member =
  Method_definition of decorator list * method_definition
| Method_signature of method_signature
| Call_static_block of statement_block
| Abstract_method_signature of abstract_method_signature
| Index_signature of index_signature
| Public_field_definition of public_field_definition

(* Method Signature

   TypeScript:

   method_signature: $ => seq(
     optional($.accessibility_modifier),
     optional('static'),
     optional($.override_modifier),
     optional('readonly'),
     optional('async'),
     optional(choice('get', 'set', '*')),
     field('name', $._property_name),
     optional('?'),
     $._call_signature),

   JavaScript:

   _property_name: $ => choice(
     alias(choice($.identifier, $._reserved_identifier),
           $.property_identifier),
     $.private_property_identifier,
     $.string,
     $.number,
     $.computed_property_name),
*)

and method_signature = {
  access: accessibility_modifier option;
  scope: method_scope;
  async: bool;
  set_get_all: set_get_all option;
  name: property_name;
  optional: bool;
  call_sig: call_signature
}

and accessibility_modifier =
  Public
| Private
| Protected

and override_modifier = Override

and set_get_all =
  Set
| Get
| All

and method_scope = {
  static: bool;
  override: bool;
  readonly: bool
}

and property_name =
  Property_identifier of identifier (* Also reserved identifiers *)
| Private_property_identifier of hash_name
| String
| Number
| Computed_property_name of expression

(* Lexical Declaration

   JavaScript:

   lexical_declaration: $ => seq(
     field('kind', choice('let', 'const')),
     commaSep1($.variable_declarator),
     $._semicolon),

   variable_declarator: $ => seq(
     field('name', choice($.identifier, $._destructuring_pattern)),
     optional($._initializer)),

   _destructuring_pattern: $ => choice($.object_pattern, $.array_pattern),

   object_pattern: $ => prec('object', seq(
     '{',
     commaSep(optional(choice(
       $.pair_pattern,
       $.rest_pattern,
       $.object_assignment_pattern,
       alias(
         choice($.identifier, $._reserved_identifier),
         $.shorthand_property_identifier_pattern)))),
     '}')),

   pair_pattern: $ => seq(
     field('key', $._property_name),
     ':',
     field('value', choice($.pattern, $.assignment_pattern))),

   rest_pattern: $ => prec.right(seq('...', $._lhs_expression)),

   _lhs_expression: $ => choice(
     $.member_expression,
     $.subscript_expression,
     $._identifier,                               // identifier + undefined
     alias($._reserved_identifier, $.identifier),
     $._destructuring_pattern),

   object_assignment_pattern: $ => seq(
     field('left', choice(
       alias(choice($._reserved_identifier, $.identifier),
                    $.shorthand_property_identifier_pattern),
       $._destructuring_pattern)),
     '=',
     field('right', $.expression)),

   array_pattern: $ => seq(
    '[', commaSep(optional(choice($.pattern, $.assignment_pattern))), ']')

   TypeScript:

   _lhs_expression: ($, previous) => choice(previous, $.non_null_expression),

   non_null_expression: $ =>
     prec.left('unary', seq($.expression, '!'))
*)

and lexical_declaration = {
  kind: let_or_const;
  decls: variable_declaration
}

and let_or_const = Let | Const

and variable_declarator =
  Var_decl_ident of identifier
| Var_decl_pattern of destructuring_pattern

and destructuring_pattern =
  Pattern_object of object_pattern
| Pattern_array  of array_pattern

and object_pattern = member_pattern list

and member_pattern =
  Member_pair_pattern of pair_pattern
| Member_rest_pattern of rest_pattern
| Member_object_assignment of object_assignment_pattern
| Member_shorthand_property of identifier (* Including reserved identifiers *)

and pair_pattern = {
  key: property_name;
  value: pair_value_pattern
}

and pair_value_pattern =
  Pair_value of pattern
| Pair_value_assignment of assignment_pattern

and rest_pattern = lhs_expression

and lhs_expression =
  Member_expression of member_expression
| Subscript_expression of subscript_expression
| Identifier of identifier (* Including reserved identifiers *)
| Undefined
| Pattern of destructuring_pattern
| Non_null_expression of expression

and object_assignment_pattern = {
  left: object_lhs_pattern;
  right: expression
}

and object_lhs_pattern = (* Isomorphic to [variable_declarator]. *)
  Object_lhs_ident of identifier (* Including reserved identifiers *)
| Object_lhs of destructuring_pattern

and array_pattern = array_cell_pattern list

and array_cell_pattern = (* Isomorphic to [pair_value_pattern]. *)
  Cell_pattern of pattern
| Cell_assignment of assignment_pattern

(* Variable Declaration

   JavaScript:

   variable_declaration: $ =>
     seq('var', commaSep1($.variable_declarator), $._semicolon),
*)

and variable_declaration = variable_declarator Nonempty_list.t

(* Abstract Class Declaration

   TypeScript:

   abstract_class_declaration: $ => prec('declaration', seq(
     repeat(field('decorator', $.decorator)),
     'abstract',
     'class',
     field('name', $._type_identifier),
     field('type_parameters', optional($.type_parameters)),
     optional($.class_heritage),
     field('body', $.class_body)))
*)

and abstract_class_declaration = {
  decorator: decorator list;
  name: type_identifier;
  type_parameters: type_parameter list;
  class_heritage: class_heritage option;
  body: class_body
}

and type_identifier = identifier

(* Method Definition

   TypeScript:

   method_definition: $ => prec.left(seq(
     optional($.accessibility_modifier),
     optional('static'),
     optional($.override_modifier),
     optional('readonly'),
     optional('async'),
     optional(choice('get', 'set', '*')),
     field('name', $._property_name),
     optional('?'),
     $._call_signature,
     field('body', $.statement_block)))
*)

and method_definition = {
  signature: method_signature;
  body: statement_block
}

and statement_block = statement list

(* Abstract Method Signature

   TypeScript:

   abstract_method_signature: $ => seq(
     optional($.accessibility_modifier),
     'abstract',
     optional($.override_modifier),
     optional(choice('get', 'set', '*')),
     field('name', $._property_name),
     optional('?'),
     $._call_signature)
*)

and abstract_method_signature = {
  access: accessibility_modifier option;
  override: override_modifier option;
  set_get_all: set_get_all option;
  name: property_name;
  optional: bool;
  call_sig: call_signature
}

(* Index Signature

   TypeScript:

   index_signature: $ => seq(
     optional(
       seq(field('sign', optional(choice('-', '+'))), 'readonly')),
     '[',
     choice(
       seq(
         field('name', choice(
           $.identifier,
           alias($._reserved_identifier, $.identifier))),
         ':',
         field('index_type', $.type)),
       $.mapped_type_clause),
     ']',
     field('type', choice(
       $.type_annotation,
       $.omitting_type_annotation,
       $.adding_type_annotation,
       $.opting_type_annotation))),

   mapped_type_clause: $ => seq(
     field('name', $._type_identifier),
     'in',
     field('type', $.type),
     optional(seq('as', field('alias', $.type)))),

   omitting_type_annotation: $ => seq('-?:', $.type),
   adding_type_annotation: $ => seq('+?:', $.type),
   opting_type_annotation: $ => seq('?:', $.type)
*)

and index_signature = {
  sign: sign option; (* readonly *)
  range: range;
  type_: index_type
}

and sign = Plus | Minus

and range =
  Typed_index_clause of typed_index_clause
| Mapped_type_clause of mapped_type_clause

and typed_index_clause = {
  name: identifier; (* Including reserved identifiers *)
  index_type: type_
}

and mapped_type_clause = {
  name: type_identifier;
  type_: type_;
  alias: type_ option
}

and index_type =
  Type_annotation of type_
| Omitting_type_annotation of type_
| Adding_type_annotation of type_
| Opting_type_annotation of type_

(* Public Field Definition

   TypeScript:

   public_field_definition: $ => seq(
     repeat(field('decorator', $.decorator)),
     optional(choice(
       seq('declare', optional($.accessibility_modifier)),
       seq($.accessibility_modifier, optional('declare')))),
     choice(
       seq(optional('static'), optional($.override_modifier),
           optional('readonly')),
       seq(optional('abstract'), optional('readonly')),
       seq(optional('readonly'), optional('abstract')), optional('accessor')),
     field('name', $._property_name),
     optional(choice('?', '!')),
     field('type', optional($.type_annotation)),
     optional($._initializer))

   JavaScript:

   _initializer: $ => seq('=', field('value', $.expression))
*)

and public_field_definition = {
  decorators: decorator list;
  access: accessibility_modifier option;
  declare: bool;
  scope: field_scope;
  name: property_name;
  mode: field_mode;
  type_: type_annotation option;
  init_value: expression
}

and field_scope = {
  static: bool;
  override: bool;
  readonly: bool;
  abstract: bool;
  accessor: bool
}

and field_mode = Optional | Definite_assert

(* Ambient Declaration

   TypeScript:

   ambient_declaration: $ => seq(
     'declare',
     choice(
       $.declaration,
       seq('global', $.statement_block),
       seq('module', '.', alias($.identifier, $.property_identifier),
           ':', $.type, $._semicolon)))
*)

and ambient_declaration =
  Declaration of declaration
| Global_declaration of statement_block
| Module_declaration of identifier * type_ (* property identifier *)

(* Enumerated Declaration

   TypeScript:

   enum_declaration: $ => seq(
     optional('const'),
     'enum',
     field('name', $.identifier),
     field('body', $.enum_body)),

   enum_body: $ => seq(
     '{',
     optional(seq(
       sepBy1(',', choice(
         field('name', $._property_name),
         $.enum_assignment)),
      optional(','))),
     '}'),

   enum_assignment: $ =>
     seq(field('name', $._property_name), $._initializer),

   _initializer: $ => seq('=', field('value', $.expression))
*)

and enum_declaration = {
  const: bool;
  name: identifier;
  body: enum_body option (* [None] denotes "{}" *)
}

and enum_body =
  Enum_name of property_name
| Enum_assignment of enum_assignment

and enum_assignment = {
  name: property_name;
  default: expression
}

(* Import Alias

   TypeScript:

   import_alias: $ => seq(
     'import',
     $.identifier,
     '=',
     choice($.identifier, $.nested_identifier),
     $._semicolon),

   JavaScript:

   nested_identifier: $ => prec('member', seq(
     field('object',
       choice($.identifier,
              alias($.nested_identifier, $.member_expression))),
     '.',
     field('property', alias($.identifier, $.property_identifier))))
*)

and import_alias = {
  alias: identifier;
  aliased: aliased
}

and aliased =
  Ident of identifier
| Nested of nested_identifier

and nested_identifier =
  identifier Nonempty_list.t * identifier (* property identifier *)

(* Interface Declaration

   TypeScript:

   interface_declaration: $ => seq(
     'interface',
     field('name', $._type_identifier),
     field('type_parameters', optional($.type_parameters)),
     optional($.extends_type_clause),
     field('body', alias($.object_type, $.interface_body))),

   extends_type_clause: $ => seq(
     'extends',
     commaSep1(field('type', choice(
       $._type_identifier,
       $.nested_type_identifier,
       $.generic_type)))),

   nested_type_identifier: $ => prec('member', seq(
     field('module', choice($.identifier, $.nested_identifier)),
     '.',
     field('name', $._type_identifier))),

   generic_type: $ => prec('call', seq(
     field('name', choice(
       $._type_identifier,
       $.nested_type_identifier)),
     field('type_arguments', $.type_arguments)))
*)

and interface_declaration = {
  name: type_identifier;
  type_parameters: type_parameter list;
  extends: extends_type_clause list;
  body: object_type (* See TYPES *)
}

and extends_type_clause =
  Extends_type of type_identifier
| Extends_nested of nested_type_identifier
| Extends_generic of generic_type

and nested_type_identifier = identifier Nonempty_list.t * type_identifier

and generic_type = generic_name * type_arguments

and generic_name =
  Generic_type of type_identifier
| Generic_nested of nested_type_identifier

(* Internal Module

   TypeScript:

   internal_module: $ => seq('namespace', $._module),

   _module: $ => prec.right(seq(
     field('name', choice($.string, $.identifier, $.nested_identifier)),
     field('body', optional($.statement_block))))
*)

and internal_module = module_

and module_ = module_name * statement_block

and module_name =
  Module_string of string
| Module_ident of identifier
| Module_nested of nested_identifier

(*  Type Alias Declaration

    TypeScript:

    type_alias_declaration: $ => seq(
      'type',
      field('name', $._type_identifier),
      field('type_parameters', optional($.type_parameters)),
      '=',
      field('value', $.type),
      $._semicolon)
*)

and type_alias_declaration = {
  name: type_identifier;
  type_parameters: type_parameter list;
  value: type_
}

(* EXPRESSIONS *)

(* JavaScript: // Simplified here

   expression: $ => choice(
     $.primary_expression,
     $.glimmer_template,
     $.assignment_expression,
     $.augmented_assignment_expression,
     $.await_expression,
     $.unary_expression,
     $.binary_expression,
     $.ternary_expression,
     $.update_expression,
     $.new_expression,
     $.yield_expression)

   TypeScript: // Simplified here

   expression: ($, previous) => {
     $.as_expression,
     $.satisfies_expression,
     $.instantiation_expression,
     $.internal_module,
     $.type_assertion}
*)

and expression =
  E_as_expression of as_expression
| E_assignment_expression of assignment_expression
| E_augmented_assignment_expression of augmented_assignment_expression
| E_await_expression of await_expression
| E_binary_expression of binary_expression
(*| E_glimmer_template of glimmer_template*) (* TODO? *)
| E_instantiation_expression of instantiation_expression
| E_internal_module of internal_module
| E_new_expression of new_expression
| E_primary_expression of primary_expression
| E_satisfies_expression of satisfies_expression
| E_ternary_expression of ternary_expression
| E_type_assertion of type_assertion
| E_unary_expression of unary_expression
| E_update_expression of update_expression
| E_yield_expression of yield_expression

(* As-expressions

   TypeScript:

   as_expression: $ => prec.left('binary', seq(
     $.expression, 'as', choice('const', $.type)))
*)

and as_expression = expression * as_what

and as_what =
  As_type of type_
| As_const

(* Assignment Expression

   TypeScript:

   assignment_expression: $ => prec.right('assign', seq(
     optional('using'),
     field('left', choice($.parenthesized_expression, $._lhs_expression)),
     '=',
     field('right', $.expression))),

   JavaScript:

   parenthesized_expression: $ => seq('(', $._expressions, ')')
*)

and assignment_expression = {
  using: bool;
  left: assignment_lhs;
  right: expression
}

and assignment_lhs =
  Assign_lhs_parens of expression
| Assign_lhs of lhs_expression

(* Member Expression

   JavaScript:

   member_expression: $ => prec('member', seq(
     field('object', choice($.expression, $.primary_expression, $.import)),
     choice('.', field('optional_chain', $.optional_chain)),
     field('property', choice(
       $.private_property_identifier,
       alias($.identifier, $.property_identifier))))),
*)

and member_expression = {
  object_: object_member;
  selector: selector;
  property: property_ident
}

and object_member =
  Object_member_expression of expression
| Object_member_primary of primary_expression
| Object_member_import

and selector = Dot | Optional_chain

and property_ident =
  Private_property_identifier of hash_name
| Property_identifier of identifier

(* Subscript Expression

   JavaScript:

   subscript_expression: $ => prec.right('member', seq(
     field('object', choice($.expression, $.primary_expression)),
     optional(field('optional_chain', $.optional_chain)),
     '[', field('index', $._expressions), ']')),

   optional_chain: _ => '?.',

   _expressions: $ => choice($.expression, $.sequence_expression),

   sequence_expression: $ => prec.right(commaSep1($.expression)),
*)

and subscript_expression = {
  object_: subscripted;
  optional_chain: optional_chain option;
  index: expressions
}

and subscripted =
  Subscripted_expression of expression
| Subscripted_primary of primary_expression

and optional_chain = Optional_chain

and expressions =
  General_expression of expression
| Sequence_expression of sequence_expression

and sequence_expression = expression list

(* Augmented Assignment Expression

   JavaScript:

   augmented_assignment_expression: $ => prec.right('assign', seq(
     field('left', $._augmented_assignment_lhs),
     field('operator', choice('+=', '-=', '*=', '/=', '%=', '^=', '&=',
                              '|=', '>>=', '>>>=', '<<=', '**=', '&&=',
                              '||=', '??=')),
     field('right', $.expression))),

   _augmented_assignment_lhs: $ => choice(
     $.member_expression,
     $.subscript_expression,
     alias($._reserved_identifier, $.identifier),
     $.identifier,
     $.parenthesized_expression)

  TypeScript:

  _augmented_assignment_lhs: ($, previous) => choice(previous,
    $.non_null_expression)
*)

and augmented_assignment_expression = {
  left: augmented_assignment_lhs;
  operator: assignment_operator;
  right: expression
}

and augmented_assignment_lhs =
  Member_expression of member_expression
| Subscript_expression of subscript_expression
| Identifier of identifier
| Parenthesized_expression of expression


and assignment_operator =
  Add_eq (* += *)
| Sub_eq (* -= *)
| Mult_eq (* *= *)
| Div_eq (* /= *)
| Rem_eq (* %= *)
| Bit_xor_eq (* ^= *)
| Bit_and_eq (* &= *)
| Bit_or_eq (* |= *)
| Bit_sr_eq (* >>= *)
| Bit_usr_eq (* >>>= *)
| Bit_sl_eq (* <<= *)
| Exponent_eq (* **= *)
| Logical_and_eq (* &&= *)
| Logical_or_eq (* ||= *)
| Non_null_eq (* ??= *)

(* Await-expression

   JavaScript:

   await_expression: $ => prec('unary_void', seq('await', $.expression))
*)

and await_expression = expression

(* Binary Expression

   JavaScript:

   binary_expression: $ => choice(
     ...[['&&', 'logical_and'],
         ['||', 'logical_or'],
         ['>>', 'binary_shift'],
         ['>>>', 'binary_shift'],
         ['<<', 'binary_shift'],
         ['&', 'bitwise_and'],
         ['^', 'bitwise_xor'],
         ['|', 'bitwise_or'],
         ['+', 'binary_plus'],
         ['-', 'binary_plus'],
         ['*', 'binary_times'],
         ['/', 'binary_times'],
         ['%', 'binary_times'],
         ['**', 'binary_exp', 'right'],
         ['<', 'binary_relation'],
         ['<=', 'binary_relation'],
         ['==', 'binary_equality'],
         ['===', 'binary_equality'],
         ['!=', 'binary_equality'],
         ['!==', 'binary_equality'],
         ['>=', 'binary_relation'],
         ['>', 'binary_relation'],
         ['??', 'ternary'],
         ['instanceof', 'binary_relation'],
         ['in', 'binary_relation']
        ].map ... // Shortened here
*)

and binary_expression =
  Logical_and (* && *)
| Logical_or  (* || *)
| Bit_sr      (* >> *)
| Bit_usr     (* >>> *)
| Bit_sl      (* << *)
| Bit_and     (* &  *)
| Bit_xor     (* ^ *)
| Bit_or      (* | *)
| Add         (* + *)
| Sub         (* - *)
| Mult        (* * *)
| Div         (* / *)
| Rem         (* % *)
| Exp         (* ** *)
| Lt          (* < *)
| Leq         (* <= *)
| Equal       (* == *)
| Strict_eq   (* === *)
| Neq         (* != *)
| Strict_neq  (* !== *)
| Geq         (* >= *)
| Gt          (* > *)
| Non_null    (* ?? *)
| Instance_of (* instanceof *)
| In          (* in *)

(* Instantiation Expression

   TypeScript:

   instantiation_expression: $ => prec('instantiation', seq(
     $.expression,
     field('type_arguments', $.type_arguments)))
*)

and instantiation_expression = expression * type_arguments

(* New-expression

   TypeScript:

   new_expression: $ => prec.right('new', seq(
     'new',
     field('constructor', $.primary_expression),
     field('type_arguments', optional($.type_arguments)),
     field('arguments', optional($.arguments))))

   JavaScript:

   arguments: $ => seq(
     '(', commaSep(optional(choice($.expression, $.spread_element))), ')'),

   spread_element: $ => seq('...', $.expression)
*)

and new_expression = {
  constructor: primary_expression;
  type_arguments: type_arguments option;
  arguments: arguments option
}

and arguments = argument list

and argument =
  Expression of expression
| Spread_element of expression

(* Statisfies-expression

   TypeScript:

   satisfies_expression: $ => prec.left('binary', seq(
     $.expression, 'satisfies', $.type))
*)

and satisfies_expression = expression * type_

(* Ternary Expression

   JavaScript:

   ternary_expression: $ => prec.right('ternary', seq(
     field('condition', $.expression),
     alias($._ternary_qmark, '?'),
     field('consequence', $.expression),
     ':',
     field('alternative', $.expression)))
*)

and ternary_expression = {
  condition: expression;
  consequence: expression;
  alternative: expression
}

(* Type Assertion

   TypeScript:

   type_assertion: $ => prec.left('unary', seq(
     $.type_arguments, $.expression))
*)

and type_assertion = type_arguments * expression

(* Unary Expression

   JavaScript:

   unary_expression: $ => prec.left('unary_void', seq(
     field('operator', choice('!', '~', '-', '+', 'typeof', 'void', 'delete')),
     field('argument', $.expression)))
*)

and unary_expression = {
  operator: unary_operator;
  argument: expression
}

and unary_operator =
  Bang
| Logical_negation
| Negation
| Typeof
| Void
| Delete

(* Update Expression

   JavaScript:

   update_expression: $ => prec.left(choice(
     seq(field('argument', $.expression),
         field('operator', choice('++', '--'))),
     seq(field('operator', choice('++', '--')),
         field('argument', $.expression))))
*)

and update_expression =
  Update_postfix of update
| Update_prefix  of update

and update = {
  argument: expression;
  operator: incr_decr_operator
}

and incr_decr_operator =
  Increment
| Decrement

(* Yield-expression

   JavaScript:

   yield_expression: $ => prec.right(seq(
     'yield',
     choice(
       seq('*', $.expression),
       optional($.expression))))
*)

and yield_expression =
  Yield of expression option
| Yield_iterable of expression

(* Primary Expression

   JavaScript:

   primary_expression: $ => choice(
     $.subscript_expression,
     $.member_expression,
     $.parenthesized_expression,
     $._identifier, // identifier + undefined
     alias($._reserved_identifier, $.identifier),
     $.this,
     $.super,
     $.number,
     $.string,
     $.template_string,
     $.regex,
     $.true,
     $.false,
     $.null,
     $.object,
     $.array,
     $.function_expression,
     $.arrow_function,
     $.generator_function,
     $.class,
     $.meta_property,
     $.call_expression)

   TypeScript:

   primary_expression: ($, previous) => choice(
     previous,
     $.non_null_expression)
*)

and primary_expression =
  E_array of array
| E_arrow_function of arrow_function
| E_call_expression of call_expression
| E_class of class_
| E_false
| E_function_expression of function_expression
| E_generator_function of generator_function
| E_identifier of identifier
| E_member_expression of member_expression
| E_meta_property of meta_property
| E_non_null_expression of expression
| E_null
| E_number
| E_object of object_
| E_parenthesized_expression of expression
(*| E_regex of regex*)
| E_string
| E_subscript_expression of subscript_expression
| E_super
| E_template_string of template_string
| E_this
| E_true
| E_undefined

(* Array Expression

   JavaScript:

   array: $ => seq(
     '[', commaSep(optional(choice($.expression, $.spread_element))), ']')
*)

and array = arguments

(* Arrow Function

   JavaScript:

   arrow_function: $ => seq(
     optional('async'),
     choice(
       field('parameter', choice(
         alias($._reserved_identifier, $.identifier),
         $.identifier,
       )),
       $._call_signature,
     ),
     '=>',
     field('body', choice($.expression, $.statement_block)))

   TypeScript:

   _call_signature: $ => seq(
     field('type_parameters', optional($.type_parameters)),
     field('parameters', $.formal_parameters),
     field('return_type', optional(
       choice($.type_annotation,
              $.asserts_annotation,
              $.type_predicate_annotation))))
*)

and arrow_function = {
  async: bool;
  parameters: parameters;
  body: function_body
}

and parameters =
  Parameter of identifier
| Call_signature of call_signature

and function_body =
  Expression of expression
| Statement_block of statement_block

(* Call Expression

   TypeScript

   call_expression: $ => choice(
     prec('call', seq(
       field('function', choice($.expression, $.import)),
       field('type_arguments', optional($.type_arguments)),
       field('arguments', choice($.arguments, $.template_string)))),
     prec('member', seq(
       field('function', $.primary_expression),
       '?.',
       field('type_arguments', optional($.type_arguments)),
       field('arguments', $.arguments))))
*)

and call_expression =
  Call of call
| Member of call_expression_member

and call_expression_member = {
  function_: primary_expression;
  type_arguments: type_arguments option;
  arguments: arguments_to_call
}

and arguments_to_call =
  Arguments of arguments
| Template_string of template_string

and call = {
  function_: fun_call;
  type_arguments: type_arguments option;
  arguments: arguments
}

and fun_call =
  Fun_call of expression
| Import

(* Function Expression

   JavaScript:

   function_expression: $ => prec('literal', seq(
     optional('async'),
     'function',
     field('name', optional($.identifier)),
     $._call_signature,
     field('body', $.statement_block)))
*)

and function_expression = {
  async: bool;
  name: identifier option;
  call_sig: call_signature;
  body: statement_block
}

(* Generator Function

   TypeScript:

   generator_function: $ => prec('literal', seq(
     optional('async'),
     'function',
     '*',
     field('name', optional($.identifier)),
     $._call_signature,
     field('body', $.statement_block)))
*)

and generator_function = function_expression

(* Metaproperty

   JavaScript:

   meta_property: _ => choice(
     seq('new', '.', 'target'),
     seq('import', '.', 'meta'))
*)

and meta_property =
  Meta_new
| Meta_import

(* Object (expression)

   JavaScript:

   object: $ => prec('object', seq(
     '{',
     commaSep(optional(choice(
       $.pair,
       $.spread_element,
       $.method_definition,
       alias(
         choice($.identifier, $._reserved_identifier),
         $.shorthand_property_identifier)))),
     '}')),

   pair: $ => seq(
     field('key', $._property_name),
     ':',
     field('value', $.expression))
*)

and object_ = object_entry list

and object_entry =
  Object_member_pair of pair
| Object_member_spread of expression
| Object_member_method of method_definition
| Object_member_shorthand of identifier

and pair = {
  key: property_name;
  value: expression
}

(* Labeled Statement

   JavaScript:

   labeled_statement: $ => prec.dynamic(-1, seq(
     field('label', alias(choice($.identifier,
                                 $._reserved_identifier),
                          $.statement_identifier)),
     ':',
     field('body', $.statement)))
*)

and labeled_statement = {
  label: identifier; (* Including reserved identifiers *)
  body: statement
}

(* Return Statement

   JavaScript:

   return_statement: $ =>
     seq('return', optional($._expressions), $._semicolon)
*)

and return_statement = expressions

(* Switch Statement

   JavaScript:

   switch_statement: $ => seq(
     'switch',
     field('value', $.parenthesized_expression),
     field('body', $.switch_body)),

   switch_body: $ =>
     seq('{', repeat(choice($.switch_case, $.switch_default)), '}'),

   switch_case: $ => seq(
     'case',
     field('value', $._expressions),
     ':',
     field('body', repeat($.statement))),

   switch_default: $ =>
     seq('default', ':', field('body', repeat($.statement)))
*)

and switch_statement = {
  value: expression;
  body: switch_body
}

and switch_body = switch_entry list

and switch_entry =
  Switch_case of switch_case
| Switch_default of switch_default

and switch_case = {
  value: expressions;
  body: statement list
}

and switch_default = statement list

(* Throw Statement

   JavaScript:

   throw_statement: $ => seq('throw', $._expressions, $._semicolon)
*)

and throw_statement = expressions

(* While Statement

   JavaScript:

   while_statement: $ => seq(
     'while',
     field('condition', $.parenthesized_expression),
     field('body', $.statement))
*)

and while_statement = {
  condition: expression;
  body: statement
}

(* With-statement

   JavaScript:

   with_statement: $ => seq(
     'with',
     field('object', $.parenthesized_expression),
     field('body', $.statement))
*)

and with_statement = {
  object_: expression;
  body: statement
}

(* PATTERNS *)

(* Pattern

   JavaScript:

   pattern: $ => prec.dynamic(-1, choice($._lhs_expression, $.rest_pattern)),
*)

and pattern =
  P_member_expression of member_expression
| P_subscript_expression of subscript_expression
| P_identifier of identifier (* Including reserved identifiers *)
| P_undefined
| P_object_pattern of object_pattern (* [destructuring_pattern] *)
| P_array_pattern of array_pattern (* [destructuring_pattern] *)
| P_non_null_expression of expression
| P_rest_pattern of rest_pattern (* [rest_pattern] *)

(* TYPES *)

(* Type

   TypeScript:

   type: $ => choice(
     $.primary_type,
     $.function_type,
     $.readonly_type,
     $.constructor_type,
     $.infer_type,
     prec(-1, alias($._type_query_member_expression_in_type_annotation,
                    $.member_expression)),
     prec(-1, alias($._type_query_call_expression_in_type_annotation,
                    $.call_expression)))
*)

and type_ =
  T_primary_type of primary_type
| T_function_type of function_type
| T_readonly_type of readonly_type
| T_constructor_type of constructor_type
| T_infer_type of infer_type
| T_member_expression of member_expression
| T_call_expression of call_expression

(* Primary Type

   TypeScript:

   primary_type: $ => choice(
     $.parenthesized_type,
     $.predefined_type,
     $._type_identifier,
     $.nested_type_identifier,
     $.generic_type,
     $.object_type,
     $.array_type,
     $.tuple_type,
     $.flow_maybe_type,
     $.type_query,
     $.index_type_query,
     alias($.this, $.this_type),
     $.existential_type,
     $.literal_type,
     $.lookup_type,
     $.conditional_type,
     $.template_literal_type,
     $.intersection_type,
     $.union_type,
     'const'),

   _type_identifier: $ => alias($.identifier, $.type_identifier),

   existential_type: _ => '*'
*)

and primary_type =
  T_parenthesized_type of type_
| T_predefined_type of predefined_type
| T_type_identifier of type_identifier
| T_nested_type_identifier of nested_type_identifier
| T_generic_type of generic_type
| T_object_type of object_type
| T_array_type of array_type
| T_tuple_type of tuple_type
| T_flow_maybe_type of primary_type
| T_type_query of type_query
| T_index_type_query of primary_type
| T_this
| T_existential_type
| T_literal_type of literal_type
| T_lookup_type of lookup_type
| T_conditional_type of conditional_type
| T_template_literal_type of template_literal_type
| T_intersection_type of intersection_type
| T_union_type of union_type
| T_const

(* Array Type

   TypeScript:

   array_type: $ => seq($.primary_type, '[', ']'),
*)

and array_type = primary_type

(* Conditional Type

   TypeScript:

   conditional_type: $ => prec.right(seq(
     field('left', $.type),
     'extends',
     field('right', $.type),
     '?',
     field('consequence', $.type),
     ':',
     field('alternative', $.type)))
*)

and conditional_type = {
  left: type_;
  right: type_;
  consequence: type_;
  alternative: type_
}

(* Intersection Type

   TypeScript:

   intersection_type: $ => prec.left(seq(optional($.type), '&', $.type))
*)

and intersection_type = type_ option * type_ (* [type_ list]? *)

(* Literal Type

   TypeScript:

   literal_type: $ => choice(
     alias($._number, $.unary_expression),
     $.number,
     $.string,
     $.true,
     $.false,
     $.null,
     $.undefined),

   _number: $ => prec.left(1, seq(
     field('operator', choice('-', '+')),
     field('argument', $.number)))

   JavaScript:

   number: _ => {
     ...
     const bigintLiteral =
       seq(choice(hexLiteral,
                  binaryLiteral,
                  octalLiteral,
                  decimalDigits),
           'n');

     return token(choice(
        hexLiteral,       // 0x12 0X12
        decimalLiteral,   // 12.5 10E2 .5 13
        binaryLiteral,    // 0b01 0B01
        octalLiteral,     // 0o12 0O12
        bigintLiteral))   // 12n 0x12n
   },
*)

and literal_type =
| T_unary_type of unary_type
| T_number of number
| T_string of string
| T_true
| T_false
| T_null
| T_undefined

and unary_type = {
  operator: sign;
  argument: number
}

and number =
  Hex_literal of hex_literal
| Dec_literal of dec_literal
| Bin_literal of bin_literal
| Oct_literal of oct_literal
| Bigint_literal of bigint_literal

(* Lookup Type

   TypeScript:

   lookup_type: $ => seq($.primary_type, '[', $.type, ']')
*)

and lookup_type = primary_type * type_

(* Object type

   TypeScript:

   object_type: $ => seq(
     choice('{', '{|'),             // {| from Flow!
     optional(seq(
       optional(choice(',', ';')),
       sepBy1(
         choice(',', $._semicolon),
         choice($.export_statement,
                $.property_signature,
                $.call_signature,
                $.construct_signature,
                $.index_signature,
                $.method_signature)),
       optional(choice(','3, $._semicolon)))),
     choice('}', '|}')),            // |} from Flow!

   property_signature: $ => seq(
     optional($.accessibility_modifier),
     optional('static'),
     optional($.override_modifier),
     optional('readonly'),
     field('name', $._property_name),
     optional('?'),
     field('type', optional($.type_annotation))),

   construct_signature: $ => seq(
     optional('abstract'),
     'new',
     field('type_parameters', optional($.type_parameters)),
     field('parameters', $.formal_parameters),
     field('type', optional($.type_annotation))),
*)

and object_type = member_type list

and member_type =
  Export_statement of export_statement (* See STATEMENTS *)
| Property_signature of property_signature
| Call_signature of call_signature
| Construct_signature of construct_signature
| Index_signature of index_signature
| Method_signature of method_signature

and property_signature = {
  access: accessibility_modifier option;
  scope: method_scope;
  name: property_name;
  optional: bool;
  type_: type_annotation option
}

and construct_signature = {
  abstract: bool;
  type_parameters: type_parameter list;
  parameters: formal_parameters;
  type_: type_annotation option
}

(* Predefined Type

   TypeScript:

   predefined_type: _ => choice(
     'any',
     'number',
     'boolean',
     'string',
     'symbol',
     alias(seq('unique', 'symbol'), 'unique symbol'),
     'void',
     'unknown',
     'string', // Repeated!
     'never',
     'object')
*)

and predefined_type =
  T_any
| T_number
| T_string
| T_symbol
| T_unique_symbol
| T_void
| T_unknown
| T_never
| T_object

(* Template Literal Type

   TypeScript:

   template_literal_type: $ => seq(
     '`',
     repeat(choice(
       alias($._template_chars, $.string_fragment),
       $.template_type)),
     '`'),

   // _template_chars???

   template_type: $ => seq('${', choice($.primary_type, $.infer_type), '}'),
*)

and template_literal_type = template_type list (* _template_chars? *)

and template_type =
  Template_primary_type of primary_type
| Template_infer_type of infer_type

(* Tuple Type

   TypeScript:

   tuple_type: $ => seq(
     '[', commaSep($._tuple_type_member), optional(','), ']'),

   _tuple_type_member: $ => choice(
     alias($.tuple_parameter, $.required_parameter),
     alias($.optional_tuple_parameter, $.optional_parameter),
     $.optional_type,
     $.rest_type,
     $.type),

   tuple_parameter: $ => seq(
     field('name', choice($.identifier, $.rest_pattern)),
     field('type', $.type_annotation)),

   optional_tuple_parameter: $ => seq(
     field('name', $.identifier),
     '?',
     field('type', $.type_annotation)),

   optional_type: $ => seq($.type, '?'),

   rest_type: $ => seq('...', $.type)
*)

and tuple_type = tuple_type_member list

and tuple_type_member =
  Tuple_parameter of tuple_parameter
| Tuple_optional_parameter of optional_tuple_parameter
| Tuple_optional_type of type_
| Tuple_rest_type of type_
| Type_type of type_

and tuple_parameter = tuple_parameter_name * type_annotation

and tuple_parameter_name =
  Tuple_parameter_ident of identifier
| Tuple_parameter_rest of rest_pattern

and optional_tuple_parameter = identifier * type_annotation

(* Type Query

   TypeScript:

   type_query: $ => prec.right(seq(
     'typeof',
     choice(
       alias($._type_query_subscript_expression, $.subscript_expression),
       alias($._type_query_member_expression, $.member_expression),
       alias($._type_query_call_expression, $.call_expression),
       alias($._type_query_instantiation_expression,
             $.instantiation_expression),
       $.identifier,
       $.this)))
*)

and type_query =
  Typeof_subscript_expression of subscript_expression
| Typeof_member_expression of member_expression
| Typeof_call_expression of call_expression
| Typeof_instantiation_expression of instantiation_expression
| Typeof_identifier of identifier
| Typeof_this

(* Union Type

   TypeScript:

   union_type: $ => prec.left(seq(optional($.type), '|', $.type))
*)

and union_type = type_ option * type_  (* [type_ list]? *)

(* Function Type

   TypeScript:

   function_type: $ => prec.left(seq(
     field('type_parameters', optional($.type_parameters)),
     field('parameters', $.formal_parameters),
     '=>',
     field('return_type', choice($.type, $.asserts, $.type_predicate)))),

   asserts: $ => seq(
     'asserts', choice($.type_predicate, $.identifier, $.this))
*)

and function_type = {
  type_parameters: type_parameter option;
  parameters: formal_parameters;
  return_type: return_type
}

and return_type =
  Return_type of type_
| Return_asserts of asserts
| Return_type_predicate of type_predicate

and asserts =
  Assert_predicate of type_predicate
| Assert_type of identifier
| Assert_this

(* Readonly Type

   TypeScript:

   readonly_type: $ => seq('readonly', $.type)
*)

and readonly_type = type_

(* Constructor Type

   TypeScript:

   constructor_type: $ => prec.left(seq(
     optional('abstract'),
     'new',
     field('type_parameters', optional($.type_parameters)),
     field('parameters', $.formal_parameters),
     '=>',
     field('type', $.type)))
*)

and constructor_type = {
  abstract: bool;
  type_parameters: type_parameter list;
  parameters: formal_parameters;
  type_: type_
}

(* Infer-type

   TypeScript:

   infer_type: $ => prec.right(seq(
     'infer',
     $._type_identifier,
     optional(seq('extends', $.type)))),
*)

and infer_type = {
  type_id: type_identifier;
  extends: type_ option
}

(* STATEMENTS

   JavaScript:

   statement: $ => choice(
     $.export_statement,
     $.import_statement,
     $.debugger_statement,
     $.expression_statement,
     $.declaration,
     $.statement_block,

     $.if_statement,
     $.switch_statement,
     $.for_statement,
     $.for_in_statement,
     $.while_statement,
     $.do_statement,
     $.try_statement,
     $.with_statement,

     $.break_statement,
     $.continue_statement,
     $.return_statement,
     $.throw_statement,
     $.empty_statement,
     $.labeled_statement),

   debugger_statement: $ => seq('debugger', $._semicolon),

   empty_statement: _ => ';'
*)

and statement =
  S_export_statement of export_statement
| S_import_statement of import_statement
| S_debugger_statement
| S_expression_statement of expression_statement
| S_declaration of declaration
| S_statement_block of statement_block

| S_if_statement of if_statement
| S_switch_statement of switch_statement
| S_for_statement of for_statement
| S_for_in_statement of for_in_statement
| S_while_statement of while_statement
| S_do_statement of do_statement
| S_try_statement of try_statement
| S_with_statement of with_statement

| S_break_statement of break_statement
| S_continue_statement of continue_statement
| S_return_statement of return_statement
| S_throw_statement of throw_statement
| S_empty_statement
| S_labeled_statement of labeled_statement

(* Break Statement

   JavaScript

   break_statement: $ => seq(
     'break',
     field('label', optional(alias($.identifier, $.statement_identifier))),
     $._semicolon)
*)

and break_statement = identifier option

(* Continue Statement

   JavaScript:

   continue_statement: $ => seq(
     'continue',
     field('label', optional(alias($.identifier, $.statement_identifier))),
     $._semicolon),
*)

and continue_statement = identifier option

(* Do-statement

   JavaScript:

   do_statement: $ => prec.right(seq(
     'do',
     field('body', $.statement),
     'while',
     field('condition', $.parenthesized_expression),
     optional($._semicolon)))
*)

and do_statement = {
  body: statement;
  condition: expression
}

(* Export Statement

   JavaScript:

   export_statement: $ => choice(
     seq('export',
         choice(
           seq('*', $._from_clause),
           seq($.namespace_export, $._from_clause),
           seq($.export_clause, $._from_clause),
           $.export_clause),
         $._semicolon),
     seq(repeat(field('decorator', $.decorator)),
         'export',
         choice(
           field('declaration', $.declaration),
           seq('default',
               choice(
                 field('declaration', $.declaration),
                 seq(field('value', $.expression), $._semicolon)))))),

   namespace_export: $ => seq('*', 'as', $._module_export_name),

   export_clause: $ =>
     seq('{', commaSep($.export_specifier), optional(','), '}'),

   export_specifier: $ => seq(
     field('name', $._module_export_name),
     optional(seq('as', field('alias', $._module_export_name)))),

   _module_export_name: $ => choice($.identifier, $.string),

   _from_clause: $ => seq('from', field('source', $.string))

   TypeScript:

   export_statement: ($, previous) => choice(
     previous,
     seq('export', 'type', $.export_clause,
         optional($._from_clause), $._semicolon),
     seq('export', '=', $.expression, $._semicolon),
     seq('export', 'as', 'namespace', $.identifier, $._semicolon))
*)

and export_statement =
  Export of export
| Export_decorator of export_decorator
| Export_type of export_clause * from_clause option
| Export_equal of expression
| Export_as_namespace of identifier

and export =
  Export_from of from_clause
| Export_as of namespace_export * from_clause
| Export_clause of export_clause * from_clause option

and from_clause = string

and namespace_export =
  Export_ident of identifier
| Export_string of string

and export_clause = export_specifier list

and export_specifier = {
  name: module_export_name;
  as_: module_export_name option
}

and module_export_name = namespace_export

and export_decorator = {
  decorators: decorator list;
  export_dec: export_dec
}

and export_dec =
  Export_declaration of declaration
| Export_default of export_default

and export_default =
  Export_default_declaration of declaration
| Export_default_expression of expression

(* Expression Statement

   TypeScript:

   expression_statement: $ => seq($._expressions, $._semicolon)
*)

and expression_statement = expressions

(* For-in/of Statement

   JavaScript:

   for_in_statement: $ => seq(
     'for',
     optional('await'),
     $._for_header,
     field('body', $.statement)),

   _for_header: $ => seq(
     '(',
     choice(
       field('left', choice(
         $._lhs_expression,
         $.parenthesized_expression)),
       seq(
         field('kind', 'var'),
         field('left', choice(
           $.identifier,
           $._destructuring_pattern)),
         optional($._initializer)),
       seq(
         field('kind', choice('let', 'const')),
         field('left', choice(
           $.identifier,
           $._destructuring_pattern)))),
     field('operator', choice('in', 'of')),
     field('right', $._expressions),
     ')')
*)

and for_in_statement = {
  await: bool;
  for_header: for_header;
  body: statement
}

and for_header = {
  range: for_range;
  operator: for_operator;
  collection: expressions
}

and for_range =
  For_in_expression of for_in_expression
| For_in_variable of for_in_variable

and for_in_expression =
  For_in_expression of lhs_expression
| For_in_parenthesized of expression

and for_in_variable = for_in_kind * for_in_var

and for_in_kind = Var | Let | Const

and for_in_var =
  For_in_ident of identifier
| For_in_pattern of destructuring_pattern

and for_operator = In | Of

(* For-statement

   JavaScript:

   for_statement: $ => seq(
     'for',
     '(',
     field('initializer', choice(
       $.lexical_declaration,
       $.variable_declaration,
       $.expression_statement,
       $.empty_statement)),
     field('condition', choice(
       $.expression_statement,
       $.empty_statement)),
     field('increment', optional($._expressions)),
     ')',
     field('body', $.statement))
*)

and for_statement = {
  initializer_: for_initializer;
  condition: for_condition;
  increment: expressions option;
  body: statement
}

and for_initializer =
  For_lexical_declaration of lexical_declaration
| For_variable_declaration of variable_declaration
| For_expression_statement of expression_statement
| For_empty_statement

and for_condition =
  For_condition_expression of expression_statement
| For_condition_empty

(* If-statement

   JavaScript:

   if_statement: $ => prec.right(seq(
     'if',
     field('condition', $.parenthesized_expression),
     field('consequence', $.statement),
     optional(field('alternative', $.else_clause)))),

   else_clause: $ => seq('else', $.statement)
*)

and if_statement = {
  condition: expression;
  consequence: statement;
  alternative: statement option
}

(* Import Statement

   TypeScript:

   import_statement: $ => seq(
     'import',
     optional(choice('type', 'typeof')),
     choice(
       seq($.import_clause, $._from_clause),
       $.import_require_clause,
       field('source', $.string)),
     optional($.import_attribute),
     $._semicolon),

   import_clause: $ => choice(
     $.namespace_import,
     $.named_imports,
     seq($._import_identifier,
         optional(seq(
           ',',
           choice(
             $.namespace_import,
             $.named_imports))))),

   namespace_import: $ => seq('*', 'as', $.identifier),

   named_imports: $ =>
     seq('{', commaSep($.import_specifier), optional(','), '}'),

   import_specifier: $ => seq(
     optional(choice('type', 'typeof')),
     choice(
       field('name', $._import_identifier),
       seq(field('name', choice($._module_export_name,
                                alias('type', $.identifier))),
           'as',
           field('alias', $._import_identifier)))),

   _import_identifier: $ => // The alias is weird, honestly.
     choice($.identifier, alias('type', $.identifier)),

   _module_export_name: $ => choice($.identifier, $.string), // See exports

   import_require_clause: $ => seq(
     $.identifier, '=', 'require', '(', field('source', $.string), ')'),

   import_attribute: $ => seq(choice('with', 'assert'), $.object),
*)

and import_statement = {
  import_kind: import_kind option;
  import: import;
  import_attribute: import_attribute option
}

and import_kind = Import_type | Import_typeof

and import =
  Import_clause of import_clause * from_clause
| Import_require_clause of import_require_clause
| Import_source of string

and import_clause =
  Import_namespace of namespace_import
| Import_named of named_imports
| Import_ident of import_identifier * namespace_or_named_import option

and namespace_or_named_import =
  Import_namespace of namespace_import
| Import_named of named_imports

and namespace_import = identifier (* "* as <ident>" *)

and named_imports = import_specifier list

and import_specifier = import_kind * import_specifier'

and import_specifier' =
  Import_spec_name of import_identifier
| Import_spec_alias of import_spec_alias

and import_identifier = identifier (* Including "type" *)

and import_spec_alias = {
  name: module_export_name;
  alias: import_identifier
}

and import_require_clause = identifier * string

and import_attribute =
  Import_with of object_
| Import_assert of object_

(* Asserts Annotation

   TypeScript:

   asserts_annotation: $ => seq(seq(':', $.asserts)), // Really?
*)

and asserts_annotation = asserts

(* Assignment Pattern

   JavaScript:

   assignment_pattern: $ => seq(
     field('left', $.pattern), '=', field('right', $.expression))
*)

and assignment_pattern = {
  left: pattern;
  right: expression
}

(* Try statement

   JavaScript

   try_statement: $ => seq(
     'try',
     field('body', $.statement_block),
     optional(field('handler', $.catch_clause)),
     optional(field('finalizer', $.finally_clause)))

   finally_clause: $ => seq('finally', field('body', $.statement_block)),

   _destructuring_pattern: $ => choice($.object_pattern, $.array_pattern)

   TypeScript:

   catch_clause: $ => seq(
     'catch',
     optional(
       seq('(',
           field('parameter',
                 choice($.identifier, $._destructuring_pattern)),
           optional(field('type', $.type_annotation)),
           ')')),
     field('body', $.statement_block))
*)

and try_statement = {
  body: statement_block;
  handler: catch_clause option;
  finalizer: finally_clause option
}

and catch_clause = {
  parameter: (catch_parameter * type_annotation option) option;
  body: statement_block
}

and catch_parameter =
  Catch_identifier of identifier
| Catch_object_pattern of object_pattern
| Catch_array_pattern of array_pattern

and finally_clause = statement_block

(* Class

   TypeScript

   class: $ => prec('literal', seq(
     repeat(field('decorator', $.decorator)),
     'class',
     field('name', optional($._type_identifier)),
     field('type_parameters', optional($.type_parameters)),
     optional($.class_heritage),
     field('body', $.class_body)))
*)

and class_ = {
  decorators: decorator list;
  name: type_identifier option;
  type_parameters: type_parameter list;
  class_heritage: class_heritage;
  body: class_member list
}

(* DECORATOR

   TypeScript:

   decorator: $ => seq(
     '@',
     choice(
       $.identifier,
       alias($.decorator_member_expression, $.member_expression),
       alias($.decorator_call_expression, $.call_expression),
       alias($.decorator_parenthesized_expression,
             $.parenthesized_expression))),

   decorator_call_expression: $ => prec('call', seq(
     field('function', choice(
       $.identifier,
       alias($.decorator_member_expression, $.member_expression))),
     optional(field('type_arguments', $.type_arguments)),
     field('arguments', $.arguments))),

   decorator_parenthesized_expression: $ => seq(
     '(',
     choice(
       $.identifier,
       alias($.decorator_member_expression, $.member_expression),
       alias($.decorator_call_expression, $.call_expression)),
     ')')

   JavaScript:

   decorator_member_expression: $ => prec('member', seq(
     field('object', choice(
       $.identifier,
       alias($.decorator_member_expression, $.member_expression))),
     '.',
     field('property', alias($.identifier, $.property_identifier))))
*)

and decorator =
  Decorator_identifier of identifier
| Decorator_member_expression of decorator_member_expression
| Decorator_call_expression of decorator_call_expression
| Decorator_parenthesized_expression of decorator_parenthesized_expression

and decorator_member_expression = {
  object_: identifier Nonempty_list.t;
  property: identifier
}

and decorator_call_expression = {
  function_: function_or_property;
  type_arguments: type_arguments option;
  arguments: arguments
}

and function_or_property =
  Function_name of identifier
| Qualified_member_expression of decorator_member_expression

and decorator_parenthesized_expression =
  Parenthesized_ident of identifier
| Parenthesized_member of decorator_member_expression
| Parenthesized_call of decorator_call_expression

(** Abstract Syntax Tree (AST) for TypeScript

   We used the JavaScript tree-sitter grammar and the TypeScript
   tree-sitter grammar as reference. The excerpts from those grammars
   are copied in a comment before the relevant AST nodes.

   For the sake of the LSP, we retain keywords in the tree.
 *)

[@@@warning "-30"] (* Duplicate record field names *)

(* DEPENDENCIES *)

type 'a ne_list = 'a Nonempty_list.t

module Wrap = Lexing_shared.Wrap
module Region = Simple_utils.Region

type 'a wrap = 'a Wrap.t

(** Literals *)
type keyword = string wrap

type symbol = string wrap
type identifier = string wrap
type string_literal = string wrap
type hash_name = string wrap
type hex_literal = (string * Hex.t) wrap
type dec_literal = (string * Q.t) wrap
type bin_literal = (string * Hex.t) wrap
type oct_literal = (string * Hex.t) wrap

type template_string =
  | String_fragment of string wrap
  | Escape_sequence of string wrap
  | Template_substitution of string wrap

type bigint_literal =
  | Hex_literal of hex_literal
  | Bin_literal of bin_literal
  | Oct_literal of oct_literal
  | Dec_literal of dec_literal

(* Compound constructs *)

type 'a enclosed = {
  opening : symbol;
  contents : 'a list;
  closing : symbol
}

type 'a braces = Braces of 'a enclosed
type 'a chevrons = Chevrons of 'a enclosed
type 'a brackets = Brackets of 'a enclosed
type 'a parens = Parens of 'a enclosed

(** The Abstract Syntax Tree

  The related grammar rule is given by:
  + JavaScript
    {@js[
     program: $ => seq(
       optional($.hash_bang_line),
       repeat($.statement))
    ]}
*)
type program = statements

and t = program
and statements = statement ne_list wrap option

(** DECLARATIONS

  Declarations, when they are valid, extend the current scope with
  new types and values (including functions).

  The related grammar rules are given by:
  + JavaScript
    {@js[
     declaration: $ => choice(
       $.function_declaration,
       $.generator_function_declaration,
       $.class_declaration,
       $.lexical_declaration,
       $.variable_declaration)
    ]}
  + TypeScript
    {@js[
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
    ]}
*)
and declaration =
  | D_function_declaration of function_declaration wrap
  | D_generator_function_declaration of generator_function_declaration wrap
  | D_class_declaration of class_declaration wrap
  | D_lexical_declaration of lexical_declaration wrap
  | D_variable_declaration of variable_declaration
  | D_function_signature of function_signature wrap
  | D_abstract_class_declaration of abstract_class_declaration wrap
  | D_module of module_ wrap
  | D_internal_module of internal_module wrap
  | D_type_alias_declaration of type_alias_declaration wrap
  | D_enum_declaration of enum_declaration wrap
  | D_interface_declaration of interface_declaration wrap
  | D_import_alias of import_alias wrap
  | D_ambient_declaration of ambient_declaration wrap

(** Function Declaration

  Function declarations introduce functions in the current scope.

  Example: {@js[function f <T>(x: T) : T { return x; };]}

  The related grammar rules are given by:
  + JavaScript
    {@js[
     function_declaration: $ => prec.right('declaration', seq(
       optional('async'), 'function',
       field('name', $.identifier),
       $._call_signature,
       field('body', $.statement_block),
       optional($._automatic_semicolon))),

     statement_block: $ => prec.right(seq(
       '{', repeat($.statement), '}', optional($._automatic_semicolon)))
    ]}

  See [function_signature] below.
*)
and function_declaration =
  { fun_sig : function_signature wrap
  ; body : statement_block
  }

(** Function Signature

  A function signature introduces a functional type in the current
  type (including type and value parameters, if any).

  Example: {@js[function f <T>(x: T) : T;]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     function_signature: $ => seq(
       optional('async'), 'function',
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
       '(', optional(seq(commaSep1($._formal_parameter), optional(','))), ')'),

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

     accessibility_modifier: _ => choice('public', 'private', 'protected'),

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
    ]}
 *)
and function_signature =
  { async : keyword option
  ; function_ : keyword
  ; name : identifier
  ; call_sig : call_signature wrap
  }

and call_signature =
  { type_parameters : type_parameters option
  ; parameters : formal_parameters
  ; return_type : call_return_type option
  }

and type_parameters = type_parameter list wrap
and formal_parameters = formal_parameter wrap list wrap

and formal_parameter =
  { parameter_name : parameter_name wrap
  ; optional : symbol option (* "?" or not *)
  ; type_ : type_annotation option
  ; default : expression option
  }

and parameter_name =
  { decorators : decorators option
  ; access : accessibility_modifier option
  ; override : keyword option
  ; readonly : keyword option
  ; pattern : parameter_pattern
  }

and decorators = decorator ne_list wrap

and parameter_pattern =
  | Parameter_pattern of pattern
  | Parameter_this of keyword

and call_return_type =
  | Type_annotation of type_annotation
  | Asserts_annotation of asserts_annotation
  | Type_predicate_annotation of type_predicate wrap

and type_annotation = symbol * type_ (* ":" *)

and type_predicate =
  { name : type_predicate_name
  ; type_ : type_
  }

and type_predicate_name =
  | Type_predicate_identifier of identifier (* Including predefined types *)
  | Type_predicate_this of keyword

(** Generator Function Declaration

  A generator function is a function whose execution can be internally
  suspended with a 'yield' instruction, and later resumed by the
  caller (at the suspension point).

  Example:
  {@js[
   function* generator(i) {
     yield i;
     yield i + 10;
   }
  ]}

  The related grammar rule is given by:
  + JavaScript
    {@js[
     generator_function_declaration: $ => prec.right('declaration', seq(
       optional('async'), 'function', '*',
       field('name', $.identifier),
       $._call_signature,
       field('body', $.statement_block),
       optional($._automatic_semicolon)))
    ]}
 *)
and generator_function_declaration = symbol (* "*" *) * function_declaration

(** Class Declaration

  A class declaration instroduces a class type in the current scope.

  Example:
  {@js[
   class Pair<T,U> {
     a : T;
     b : U;
     constructor (a: T, b: U) {
       this.a = a;
       this.b = b
     }
   }
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
     ]}
   + JavaScript
     {@js[
      class_static_block: $ => seq(
        'static',
        optional($._automatic_semicolon),
        field('body', $.statement_block))
     ]}
 *)
and class_declaration =
  { decorators : decorators option
  ; class_ : keyword
  ; name : type_identifier
  ; type_parameters : type_parameters
  ; class_heritage : class_heritage option
  ; body : class_body
  }

and class_heritage =
  | Extends_clause of extends_clause * implements_clause option
  | Implements_clause of implements_clause

and extends_clause = extends_clause_single ne_list wrap

and extends_clause_single =
  { value : expression
  ; type_arguments : type_arguments option
  }

and type_arguments = type_ ne_list wrap
and implements_clause = type_ ne_list wrap

and type_parameter =
  { const : keyword option
  ; name : type_identifier
  ; constraint_ : type_ option
  ; value : type_ option (* default *)
  }

and class_body = class_member list wrap

and class_member =
  | Method_definition of decorators option * method_definition
  | Method_signature of method_signature wrap
  | Call_static_block of statement_block
  | Abstract_method_signature of abstract_method_signature wrap
  | Index_signature of index_signature wrap
  | Public_field_definition of public_field_definition wrap

(** Method Signature

  A method signature is the type of a method in a class.

  Example: {@js[public m? (x: number) : number]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     method_signature: $ => seq(
       optional($.accessibility_modifier),
       optional('static'),
       optional($.override_modifier),
       optional('readonly'),
       optional('async'),
       optional(choice('get', 'set', '*')),
       field('name', $._property_name),
       optional('?'),
       $._call_signature)
    ]}
  + JavaScript
    {@js[
     _property_name: $ => choice(
       alias(choice($.identifier, $._reserved_identifier),
                    $.property_identifier),
       $.private_property_identifier,
       $.string,
       $.number,
       $.computed_property_name),

     computed_property_name: $ => seq('[', $.expression, ']')
    ]}
*)
and method_signature =
  { access : accessibility_modifier option
  ; scope : method_scope wrap
  ; async : keyword option
  ; set_get_all : set_get_all option
  ; name : property_name
  ; optional : symbol option (* "?" *)
  ; call_sig : call_signature wrap
  }

and accessibility_modifier =
  | Public of keyword
  | Private of keyword
  | Protected of keyword

and set_get_all =
  | Set of keyword
  | Get of keyword
  | All of symbol

and method_scope =
  { static : keyword option
  ; override : keyword option
  ; readonly : keyword option
  }

and property_name =
  | Property_identifier of identifier (* Also reserved identifiers *)
  | Private_property_identifier of private_property_identifier
  | String of string_literal
  | Number of bigint_literal (* Sign? *)
  | Computed_property_name of expression

and private_property_identifier = hash_name

(** Lexical Declaration

  Lexical declarations are declarations of let- or
  const-variables. When achieved by means of object & array patterns,
  the variables they contain are introduced in the current scope.

  Example: {@js[const {x, y} = z;]}

  The related grammar rules are given by:
  + JavaScript
    {@js[
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
         alias(choice($.identifier, $._reserved_identifier),
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
    }]
  + TypeScript
    {@js[
     _lhs_expression: ($, previous) => choice(previous, $.non_null_expression),

     non_null_expression: $ =>
       prec.left('unary', seq($.expression, '!'))
    ]}
 *)
and lexical_declaration =
  { kind : let_or_const
  ; decls : variable_declaration
  }

and let_or_const =
  | Let of keyword
  | Const of keyword

and variable_declarator = lhs_pattern

and lhs_pattern =
  | Decl_ident of identifier
  | Decl_pattern of destructuring_pattern

and destructuring_pattern =
  | Pattern_object of object_pattern
  | Pattern_array of array_pattern

and object_pattern = member_pattern list wrap

and member_pattern =
  | Member_pair_pattern of pair_pattern wrap
  | Member_rest_pattern of rest_pattern
  | Member_object_assignment of object_assignment_pattern wrap
  | Member_shorthand_property of identifier (* Including reserved identifiers *)

and pair_pattern =
  { key : property_name
  ; value : pair_value_pattern
  }

and pair_value_pattern =
  | Pair_value of pattern
  | Pair_value_assignment of assignment_pattern

and rest_pattern = lhs_expression

and lhs_expression =
  | Member_expression of member_expression
  | Subscript_expression of subscript_expression
  | Identifier of identifier (* Including reserved identifiers *)
  | Undefined of keyword
  | Pattern of destructuring_pattern
  | Non_null_expression of expression

and object_assignment_pattern =
  { left : object_lhs_pattern
  ; right : expression
  }

and object_lhs_pattern = lhs_pattern
and array_pattern = array_cell_pattern list wrap

and array_cell_pattern =
  (* Isomorphic to [pair_value_pattern]. *)
  | Cell_pattern of pattern
  | Cell_assignment of assignment_pattern wrap

(** Variable Declaration

  Variable declarations introduce in the current scope mutable
  variable.

  Example: {@js[var x : number = 5;]}

  The related grammar rule is given by:
  + JavaScript
    {@js[
     variable_declaration: $ =>
       seq('var', commaSep1($.variable_declarator), $._semicolon),
    ]}
 *)
and variable_declaration = variable_declarator ne_list wrap (* TODO: "var"? *)

(** Abstract Class Declaration

  An abstract class declaration is the declaration of a class that
  cannot be instantiated (no public constructors), and are instead
  used as a base to derive other classes, enforcing this way some
  method implementations and the presence of certain members with
  certain types.

  Example:
  {@js[
   abstract class Base {
     abstract getName(): string;

     printName() {
       console.log("Hello, " + this.getName());
     }
   }
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     abstract_class_declaration: $ => prec('declaration', seq(
       repeat(field('decorator', $.decorator)),
       'abstract',
       'class',
       field('name', $._type_identifier),
       field('type_parameters', optional($.type_parameters)),
       optional($.class_heritage),
       field('body', $.class_body)))
    }]
 *)
and abstract_class_declaration =
  { decorators : decorators option
  ; abstract : keyword
  ; class_ : keyword
  ; name : type_identifier
  ; type_parameters : type_parameters
  ; class_heritage : class_heritage option
  ; body : class_body
  }

and type_identifier = identifier

(** Method Definition

  A method is defined in a class, where it is introduced to its scope
  (access can be modified).

  Example: [@js{public static id? <T>(x: T) : T { return x; }}]

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
    }]
 *)
and method_definition =
  { signature : method_signature
  ; body : statement_block
  }

and statement_block = statements

(** Abstract Method Signature

  Abstract methods are methods without a body, and thus belonging to
  an abstract class.

  Example:
  {@js[
   abstract class Base {
     abstract getName(): string;

     printName() {
       console.log("Hello, " + this.getName());
     }
   }
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     abstract_method_signature: $ => seq(
       optional($.accessibility_modifier),
       'abstract',
       optional($.override_modifier),
       optional(choice('get', 'set', '*')),
       field('name', $._property_name),
       optional('?'),
       $._call_signature)
    ]}
 *)
and abstract_method_signature =
  { access : accessibility_modifier option
  ; override : keyword option
  ; set_get_all : set_get_all option
  ; name : property_name
  ; optional : symbol option (* '?' *)
  ; call_sig : call_signature
  }

(** Index Signature

  Classes and interfaces can declare an index signature, that is, the
  functional type of the index operator '[]' when applied to an
  instance of the class (as if an array).

  Example:
  {@js[
   interface StringArray {
     [index: number]: string;
   }
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
    ]}
 *)
and index_signature =
  { sign : sign option (* readonly *)
  ; range : index_range
  ; type_ : index_type
  }

and sign =
  | Plus of symbol
  | Minus of symbol

and index_range =
  | Typed_index_clause of typed_index_clause wrap
  | Mapped_type_clause of mapped_type_clause wrap

and typed_index_clause =
  { name : identifier (* Including reserved identifiers *)
  ; index_type : type_
  }

and mapped_type_clause =
  { name : type_identifier
  ; type_ : type_
  ; alias : type_ option
  }

and index_type =
  | Type_annotation of type_
  | Omitting_type_annotation of type_
  | Adding_type_annotation of type_
  | Opting_type_annotation of type_

(** Public Field Definition

  A public field definition is a property definition (variables and
  methods). Contrary to what the name indicates, the access to the
  property can be private or protected.

  Example:
  {@js[
   class ClassWithStaticMethod {
     static staticProperty = 'someValue';
   }
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     public_field_definition: $ => seq(
       repeat(field('decorator', $.decorator)),
       optional(choice(
         seq('declare', optional($.accessibility_modifier)),
         seq($.accessibility_modifier, optional('declare')))), // private??
       choice(
         seq(optional('static'), optional($.override_modifier),
             optional('readonly')),
         seq(optional('abstract'), optional('readonly')),
         seq(optional('readonly'), optional('abstract')),
             optional('accessor')),
       field('name', $._property_name),
       optional(choice('?', '!')),
       field('type', optional($.type_annotation)),
       optional($._initializer))
    ]}
  + JavaScript
    {@js[
     _initializer: $ => seq('=', field('value', $.expression))
    ]}
 *)
and public_field_definition =
  { decorators : decorators option
  ; access : accessibility_modifier option
  ; declare : keyword option
  ; scope : field_scope
  ; name : property_name
  ; mode : field_mode option
  ; type_ : type_annotation option
  ; init_value : expression
  }

and field_scope =
  { static : keyword option
  ; override : keyword option
  ; readonly : keyword option
  ; abstract : keyword option
  ; accessor : keyword option
  }

and field_mode =
  | Optional of symbol (* "?" *)
  | Definite_assert of symbol (* "!" *)

(** Ambient Declaration

  An ambient declaration is a declaration of a value or function that
  is defined externally to the project (a library, for example).

  Example:
  {@js[
   declare const myVar: string;
  ]}

  The related grammar rule is given by:
  + TypeScript
    {@js[
     ambient_declaration: $ => seq(
       'declare',
       choice(
         $.declaration,
         seq('global', $.statement_block),
         seq('module', '.', alias($.identifier, $.property_identifier),
             ':', $.type, $._semicolon)))
    ]}
 *)
and ambient_declaration =
  | Declaration of declaration
  | Global_declaration of statement_block
  | Module_declaration of identifier * type_ (* property identifier *)

(** Enumerated Declaration

  An enumerated declaration introduces in the current type environment
  a type defined by the finite union of some values, which are
  implicitly or explicitly mapped to unique integers.

  Example:
  {@js[
   const enum Direction {Up = 1, Down, Left, Right,}
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
    }]
 *)
and enum_declaration =
  { const : keyword option
  ; name : identifier
  ; body : enum_body option (* [None] denotes "{}" *)
  }

and enum_body =
  | Enum_name of property_name
  | Enum_assignment of enum_assignment wrap

and enum_assignment =
  { name : property_name
  ; default : expression
  }

(** Import Alias

  Import aliases introduce a new name for a (possibly qualified)
  namespace or property.

  Example:
  {@[
   import n = p.q.r.s;
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     import_alias: $ => seq(
       'import',
       $.identifier,
       '=',
       choice($.identifier, $.nested_identifier),
       $._semicolon),
    ]}
  + JavaScript
    {@js[
     nested_identifier: $ => prec('member', seq(
       field('object',
         choice($.identifier,
                alias($.nested_identifier, $.member_expression))),
       '.',
       field('property', alias($.identifier, $.property_identifier))))
    ]}
 *)
and import_alias =
  { import : keyword
  ; alias : identifier
  ; aliased : aliased
  }

and aliased =
  | Ident of identifier
  | Nested of nested_identifier

and nested_identifier =
  (identifier ne_list wrap * identifier) wrap (* property identifier *)

(** Interface Declaration

  An interface is similar to an abstract class, except it cannot
  contain any implementation: it's more akin to a type. Interfaces can
  be generic and extend other interfaces (multiple inheritance).

  Example:
  {@js[
   interface SquareConfig {
     color?: string;
     width?: number;
   }
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
    }]
 *)
and interface_declaration =
  { name : type_identifier
  ; type_parameters : type_parameters
  ; extends : extends_type_clause list
  ; body : object_type (* See TYPES *)
  }

and extends_type_clause =
  | Extends_type of type_identifier
  | Extends_nested of nested_type_identifier
  | Extends_generic of generic_type

and nested_type_identifier = (identifier ne_list wrap * type_identifier) wrap
and generic_type = generic_name * type_arguments

and generic_name =
  | Generic_type of type_identifier
  | Generic_nested of nested_type_identifier

(** Internal Module

  Internal modules, or namespaces, create a scope made of type and
  value declarations.

  Example:
  {@js[
   namespace Validation {
     export interface StringValidator {
       isAcceptable(s: string): boolean;
     }
   }
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     internal_module: $ => seq('namespace', $._module),

     _module: $ => prec.right(seq(
       field('name', choice($.string, $.identifier, $.nested_identifier)),
       field('body', optional($.statement_block))))
    ]}
 *)
and internal_module = module_

and module_ = (module_name * statement_block) wrap

and module_name =
  | Module_string of string
  | Module_ident of identifier
  | Module_nested of nested_identifier

(**  Type Alias Declaration

  Type aliases introduce in the current scope a new type name that
  denotes a type expression.

  Example:
  {@js[
   type t = number;
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     type_alias_declaration: $ => seq(
       'type',
       field('name', $._type_identifier),
       field('type_parameters', optional($.type_parameters)),
       '=',
       field('value', $.type),
       $._semicolon)
    ]}
 *)
and type_alias_declaration =
  { name : type_identifier
  ; type_parameters : type_parameters
  ; value : type_
  }

(** EXPRESSIONS

  The related grammar rules are given by:
  + JavaScript
    {@js[
     expression: $ => choice( // Simplified here
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
    ]}
  + TypeScript
    {@js[
     expression: ($, previous) => { // Simplified here
       $.as_expression,
       $.satisfies_expression,
       $.instantiation_expression,
       $.internal_module,
       $.type_assertion}
    ]}
*)
and expression =
  | E_as_expression of as_expression
  | E_assignment_expression of assignment_expression
  | E_augmented_assignment_expression of augmented_assignment_expression
  | E_await_expression of await_expression
  | E_binary_expression of binary_expression
  (*| E_glimmer_template of glimmer_template*)
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

(** As-expressions

  As-expressions are expressions whose type is explicitly constrained
  by an annotation (that is, a so-called type assertion).

  Example:
  {@js[
   let a = '123';
   let n = a as number;
   const m = a as const;
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     as_expression: $ => prec.left('binary', seq(
       $.expression, 'as', choice('const', $.type)))
    ]}
 *)
and as_expression = (expression * as_what) wrap

and as_what =
  | As_type of type_
  | As_const of keyword

(** Assignment Expression

  Assignments to mutable variables.

  Example:
  {@js[
   x = 5;
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     assignment_expression: $ => prec.right('assign', seq(
       optional('using'),
       field('left', choice($.parenthesized_expression, $._lhs_expression)),
       '=',
       field('right', $.expression)))
    ]}
  + JavaScript
    {@js[
     parenthesized_expression: $ => seq('(', $._expressions, ')')
    ]}
*)
and assignment_expression =
  { using : keyword option
  ; left : assignment_lhs
  ; right : expression
  }

and assignment_lhs =
  | Assign_lhs_parens of expression
  | Assign_lhs of lhs_expression

(** Member Expression

  A member expression is the selection of a property in an object.

  Example:
  {@js[
   x.m
  ]}

  The related grammar rules are given by:
  + JavaScript
    {@js[
     member_expression: $ => prec('member', seq(
       // Why primary_expression since it is included in expression?
       field('object', choice($.expression, $.primary_expression, $.import)),
       choice('.', field('optional_chain', $.optional_chain)),
       field('property', choice(
         $.private_property_identifier,
         alias($.identifier, $.property_identifier))))),

     optional_chain: _ => '?.'
    ]}
*)
and member_expression =
  { object_ : object_member
  ; selector : selector
  ; property : property_ident
  }

and object_member =
  | Object_member_expression of expression
  | Object_member_primary of primary_expression
  | Object_member_import

and selector =
  | Dot
  | Optional_chain

and property_ident =
  | Private_property_identifier of hash_name
  | Property_identifier of identifier

(** Subscript Expression

  Projecting arrays.

  Example:
  {@js[
  f(x).[n]
  ]}

  The related grammar rules are given by:
  + JavaScript
    {@js[
     subscript_expression: $ => prec.right('member', seq(
       // Why primary_expression since it is included in expression?
       field('object', choice($.expression, $.primary_expression)),
       optional(field('optional_chain', $.optional_chain)),
       '[', field('index', $._expressions), ']')),

     optional_chain: _ => '?.',

     _expressions: $ => choice($.expression, $.sequence_expression),

     sequence_expression: $ => prec.right(commaSep1($.expression))
    ]}
*)
and subscript_expression =
  { object_ : subscripted
  ; optional_chain : optional_chain option
  ; index : expressions
  }

and subscripted =
  | Subscripted_expression of expression
  | Subscripted_primary of primary_expression

and optional_chain = Optional_chain

and expressions =
  | General_expression of expression
  | Sequence_expression of sequence_expression

and sequence_expression = expression ne_list wrap

(** Augmented Assignment Expression

  Assignments can be composed with an arithmetic, logical or bitwise
  operator.

  Example:
  {@js[
   x += 4;
  ]}

  The related grammar rules are given by:
  + JavaScript
    {@js[
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
    ]}
  + TypeScript
    {@js[
     _augmented_assignment_lhs: ($, previous) => choice(previous,
       $.non_null_expression)
    ]}
*)
and augmented_assignment_expression =
  { left : augmented_assignment_lhs
  ; operator : assignment_operator
  ; right : expression
  }

and augmented_assignment_lhs =
  | Member_expression of member_expression
  | Subscript_expression of subscript_expression
  | Identifier of identifier
  | Parenthesized_expression of expression

and assignment_operator =
  | Add_eq (* += *)
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

(** Await-expression

   Expressions introduced by the "await" keyword are promises used
   only inside asynchronous functions (introduced by the "async"
   keyword). Their evaluation pauses the embedding async function, but
   do not block the main thread, so the caller can resume, until the
   promise is either fulfilled or rejected, and handled by the async
   function.

   Example:
   {@js[
    async function foo(name) {
      console.log(name, "start");
      await console.log(name, "middle");
      console.log(name, "end");
    }

    foo("First");
    foo("Second");

    // First start
    // First middle
    // Second start
    // Second middle
    // First end
    // Second end
   ]}

  The related grammar rules are given by::
  + JavaScript
    {@js[
     await_expression: $ => prec('unary_void', seq('await', $.expression))
    ]}
 *)
and await_expression = expression

(** Binary Expression

  The related grammar rules are given by:
  + JavaScript
   {@js[
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
         ].map(([operator, precedence, associativity]) =>
        (associativity === 'right' ? prec.right : prec.left)(precedence, seq(
          field('left', operator === 'in' ? choice($.expression, $.private_property_identifier) : $.expression),
          field('operator', operator),
          field('right', $.expression)))))
   ]}
*)
and binary_expression =
  | Logical_and (** && *)
  | Logical_or (** || *)
  | Bit_sr (** >> *)
  | Bit_usr (** >>> *)
  | Bit_sl (** << *)
  | Bit_and (** &  *)
  | Bit_xor (** ^ *)
  | Bit_or (** | *)
  | Add (** + *)
  | Sub (** - *)
  | Mult (** * *)
  | Div (** / *)
  | Rem (** % *)
  | Exp (** ** *)
  | Lt (** < *)
  | Leq (** <= *)
  | Equal (** == *)
  | Strict_eq (** === *)
  | Neq (** != *)
  | Strict_neq (** !== *)
  | Geq (** >= *)
  | Gt (** > *)
  | Non_null (** ?? *)
  | Instance_of (** instanceof *)
  | In (** in *)

(** Instantiation Expression

  Instantiation expressions are a way to instantiate the type
  parameters of a generic function.

  Example:
  {@js[
   const generic = <C, A>(c: C, a: A) => {
     // do something
     return { c, a }
   };
   const specialised = generic<string, number>;
  ]}

  The related grammar rules are given by:
  + TypeScript
   {@js[
    instantiation_expression: $ => prec('instantiation', seq(
      $.expression,
      field('type_arguments', $.type_arguments)))
   ]}
 *)
and instantiation_expression = expression * type_arguments

(** New-expression

  New-expressions are simply the instantiation of an object by calling
  a constructor.

  Example:
  {@js[
  const c = new C<number>(4);
  ]}

  The related grammar rules are given by:
  + TypeScript
   {@js[
    new_expression: $ => prec.right('new', seq(
      'new',
      field('constructor', $.primary_expression),
      field('type_arguments', optional($.type_arguments)),
      field('arguments', optional($.arguments))))
   ]}
  + JavaScript
   {@js[
    arguments: $ => seq(
      '(', commaSep(optional(choice($.expression, $.spread_element))), ')'),

    spread_element: $ => seq('...', $.expression)
   ]}
 *)
and new_expression =
  { constructor : primary_expression
  ; type_arguments : type_arguments option
  ; arguments : arguments option
  }

and arguments = argument list

and argument =
  | Expression of expression
  | Spread_element of expression

(** Satisfies-expression

  The 'satisfies' binary operator is like an `as` operator: it brings
  together an expression and a type. The difference is that the former
  does not change the type of the expression, only checks its
  compatibility.

  Example:
  {@js[
   type Colors = "red" | "green" | "blue";
   type RGB = [red: number, green: number, blue: number];
   const palette = {
     red: [255, 0, 0],
     green: "#00ff00",
     blue: [0, 0, 255]
   } satisfies Record<Colors, string | RGB>;
   const greenNormalized = palette.green.toUpperCase();
  ]}

  The related grammar rules are given by:
  + TypeScript
   {@js[
    satisfies_expression: $ => prec.left('binary', seq(
      $.expression, 'satisfies', $.type))
   ]}
 *)
and satisfies_expression = expression * type_

(** Ternary Expression

  The ternary conditional expression (equivalent to if-else as an
  expression).

  Example:
  {@js[
   const positive_logic = true ? 1 : 0;
  ]}

  The related grammar rules are given by:
  + JavaScript
   {@js[
    ternary_expression: $ => prec.right('ternary', seq(
      field('condition', $.expression),
      alias($._ternary_qmark, '?'),
      field('consequence', $.expression),
      ':',
      field('alternative', $.expression)))
   ]}
 *)
and ternary_expression =
  { condition : expression
  ; consequence : expression
  ; alternative : expression
  }

(** Type Assertion

  The related grammar rules are given by:
  + TypeScript
   {@js[
    type_assertion: $ => prec.left('unary', seq(
      $.type_arguments, $.expression))
   ]}
*)
and type_assertion = type_arguments * expression

(** Unary Expression

  The related grammar rules are given by:
  + JavaScript
   {@js[
    unary_expression: $ => prec.left('unary_void', seq(
      field('operator', choice('!', '~', '-', '+', 'typeof', 'void', 'delete')),
      field('argument', $.expression)))
   ]}
*)
and unary_expression =
  { operator : unary_operator
  ; argument : expression
  }

and unary_operator =
  | Bang
  | Logical_negation
  | Negation
  | Typeof
  | Void
  | Delete

(** Update Expression

  Incrementing or decrementing a variable.

  The related grammar rules are given by:
  + JavaScript
   {@js[
    update_expression: $ => prec.left(choice(
      seq(field('argument', $.expression),
          field('operator', choice('++', '--'))),
      seq(field('operator', choice('++', '--')),
          field('argument', $.expression))))
   ]}
*)
and update_expression =
  | Update_postfix of update
  | Update_prefix of update

and update =
  { argument : expression
  ; operator : incr_decr_operator
  }

and incr_decr_operator =
  | Increment
  | Decrement

(** Yield-expression

  A generator function is a function whose execution can be internally
  suspended with a 'yield' instruction, and later resumed by the
  caller (at the suspension point).

  Example:
  {@js[
   function* generator(i) {
     yield i;
     yield i + 10;
   }
  ]}

  The related grammar rules are given by:
  + JavaScript
   {@js[
    yield_expression: $ => prec.right(seq(
      'yield',
      choice(seq('*', $.expression),
             optional($.expression))))
   ]}
*)
and yield_expression =
  | Yield of expression option
  | Yield_iterable of expression

(** Primary Expression

  The related grammar rules are given by:
  + JavaScript
    {@js[
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
      $.call_expression),

    template_string: $ => seq(
      '`',
      repeat(choice(
        alias($._template_chars, $.string_fragment),
        $.escape_sequence,
        $.template_substitution)),
      '`'),

    template_substitution: $ => seq('${', $._expressions, '}')
   }]
  + TypeScript
   {@js[
    primary_expression: ($, previous) => choice(
      previous,
      $.non_null_expression)
   ]}
*)
and primary_expression =
  | E_array of array
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
  | E_regex of string
  | E_string
  | E_subscript_expression of subscript_expression
  | E_super
  | E_template_string of template_string
  | E_this
  | E_true
  | E_undefined

(** Array Expression

  Example:
  {@js[
   x[1,n]
  ]}

  The related grammar rules are given by:
  + JavaScript
   {@js[
    array: $ => seq(
      '[', commaSep(optional(choice($.expression, $.spread_element))), ']')
   ]}
*)
and array = arguments

(** Arrow Function

  Functional expressions.

  Example:
  {@js[
   x => x + 1
  ]}

  The related grammar rules are given by:
  + JavaScript:
   {@js[
    arrow_function: $ => seq(
      optional('async'),
      choice(
        field('parameter', choice(
          alias($._reserved_identifier, $.identifier),
          $.identifier,)),
        $._call_signature),
      '=>',
      field('body', choice($.expression, $.statement_block)))
   ]}
*)
and arrow_function =
  { async : bool
  ; parameters : parameters
  ; body : function_body
  }

and parameters =
  | Parameter of identifier
  | Call_signature of call_signature

and function_body =
  | Expression of expression
  | Statement_block of statement_block

(** Call Expression

  The related grammar rules are given by:
  + TypeScript
   {@js[
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
   ]}
*)
and call_expression =
  | Call of call
  | Member of call_expression_member

and call_expression_member =
  { function_ : primary_expression
  ; type_arguments : type_arguments option
  ; arguments : arguments_to_call
  }

and arguments_to_call =
  | Arguments of arguments
  | Template_string of template_string

and call =
  { function_ : fun_call
  ; type_arguments : type_arguments option
  ; arguments : arguments
  }

and fun_call =
  | Fun_call of expression
  | Import

(** Function Expression

  The related grammar rules are given by:
  + JavaScript
   {@js[
    function_expression: $ => prec('literal', seq(
      optional('async'), 'function',
      field('name', optional($.identifier)),
      $._call_signature,
      field('body', $.statement_block)))
   ]}
*)
and function_expression =
  { async : bool
  ; name : identifier option
  ; call_sig : call_signature
  ; body : statement_block
  }

(** Generator Function

  A generator function is a function whose execution can be internally
  suspended with a 'yield' instruction, and later resumed by the
  caller (at the suspension point). The difference with a declaration
  is that the generator can be anonymous, as it is an expression.

  The related grammar rules are given by:
  + TypeScript
    {@js[
     generator_function: $ => prec('literal', seq(
       optional('async'), 'function', '*',
       field('name', optional($.identifier)),
       $._call_signature,
       field('body', $.statement_block)))
    ]}
 *)
and generator_function = function_expression

(** Metaproperty

  The related grammar rules are given by:
  + JavaScript
   {@js[
    meta_property: _ => choice(
      seq('new', '.', 'target'),
      seq('import', '.', 'meta'))
   ]}
*)
and meta_property =
  | Meta_new_target
  | Meta_import_meta

(** Object (expression)

  Example:
  {@js[
   {x: 5}
  ]}

  The related grammar rules are given by:
  + JavaScript
   {@js[
    object: $ => prec('object', seq(
      '{',
      commaSep(optional(choice(
        $.pair,
        $.spread_element,
        $.method_definition,
        alias(choice($.identifier, $._reserved_identifier),
              $.shorthand_property_identifier)))),
      '}')),

    pair: $ => seq(
      field('key', $._property_name),
      ':',
      field('value', $.expression))
   ]}
*)
and object_ = object_entry list

and object_entry =
  | Object_member_pair of pair
  | Object_member_spread of expression
  | Object_member_method of method_definition
  | Object_member_shorthand of identifier

and pair =
  { key : property_name
  ; value : expression
  }

(** Labeled Statement

  The related grammar rule is given by:
  + JavaScript
   {@js[
    labeled_statement: $ => prec.dynamic(-1, seq(
      field('label', alias(choice($.identifier,
                                  $._reserved_identifier),
                           $.statement_identifier)),
      ':',
      field('body', $.statement)))
   ]}
*)
and labeled_statement =
  { label : identifier (* Including reserved identifiers *)
  ; body : statement
  }

(** Return Statement

  The related grammar rule is given by:
  + JavaScript
   {@js[
    return_statement: $ =>
      seq('return', optional($._expressions), $._semicolon)
   ]}
*)
and return_statement =
  { kwd_return : keyword
  ; expressions : expressions option
  }

(** Switch Statement

  The related grammar rules are given by:
  + JavaScript
   {@js[
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
   ]}
*)
and switch_statement =
  { kwd_switch : keyword
  ; value : expression
  ; body : switch_body
  }

and switch_body = switch_entry braces

and switch_entry =
  | Switch_case of switch_case
  | Switch_default of switch_default

and switch_case =
  { value : expressions
  ; body : statement list
  }

and switch_default = statement list

(** Throw Statement

  The related grammar rule is given by:
  + JavaScript
    {@js[
     throw_statement: $ => seq('throw', $._expressions, $._semicolon)
    ]}
*)
and throw_statement =
  { kwd_throw : keyword
  ; expressions : expressions
  }

(** While Statement

  The related grammar rule is given by:
  + JavaScript
    {@js[
     while_statement: $ => seq(
       'while',
       field('condition', $.parenthesized_expression),
       field('body', $.statement))
    ]}
*)
and while_statement =
  { kwd_while : keyword
  ; condition : expression
  ; body : statement
  }

(** With-statement

  The related grammar rule is given by:
  + JavaScript
    {@js[
     with_statement: $ => seq(
       'with',
       field('object', $.parenthesized_expression),
       field('body', $.statement))
    ]}
*)
and with_statement =
  { kwd_with : keyword
  ; object_ : expression
  ; body : statement
  }

(** PATTERNS

  The related grammar rule is given by:
  + JavaScript
    {@js[
     pattern: $ => prec.dynamic(-1, choice($._lhs_expression, $.rest_pattern))
    ]}
*)
and pattern =
  | P_member_expression of member_expression
  | P_subscript_expression of subscript_expression
  | P_identifier of identifier (* Including reserved identifiers *)
  | P_undefined
  | P_object_pattern of object_pattern (* [destructuring_pattern] *)
  | P_array_pattern of array_pattern (* [destructuring_pattern] *)
  | P_non_null_expression of expression
  | P_rest_pattern of rest_pattern (* [rest_pattern] *)

(** TYPES

  The related grammar rule is given by:
  + TypeScript
    {@js[
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
    ]}
*)
and type_ =
  | T_primary_type of primary_type
  | T_function_type of function_type
  | T_readonly_type of readonly_type
  | T_constructor_type of constructor_type
  | T_infer_type of infer_type
  | T_member_expression of member_expression
  | T_call_expression of call_expression

(** Primary Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
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

     parenthesized_type: $ => seq('(', $.type, ')'),

     _type_identifier: $ => alias($.identifier, $.type_identifier),

     existential_type: _ => '*',

     index_type_query: $ => seq('keyof', $.primary_type)
    ]}
*)
and primary_type =
  | T_parenthesized_type of type_
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

(** Array Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
     array_type: $ => seq($.primary_type, '[', ']')
    ]}
*)
and array_type = primary_type

(** Conditional Type

    Conditional types are akin to the ternary conditional statements on
    values. They are a form of test on a type, based on inheritance.

    Example:
    {@js[
     interface Animal {
       live(): void;
     }
     interface Dog extends Animal {
       woof(): void;
     }
     type Example1 = Dog extends Animal ? number : string;
     // type Example1 = number
    ]}

    The related grammar rule is given by:
    + TypeScript
    {@js[
     conditional_type: $ => prec.right(seq(
       field('left', $.type),
       'extends',
       field('right', $.type),
       '?',
       field('consequence', $.type),
       ':',
       field('alternative', $.type)))
    ]}
*)
and conditional_type =
  { left : type_
  ; right : type_
  ; consequence : type_
  ; alternative : type_
  }

(** Intersection Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
     intersection_type: $ => prec.left(seq(optional($.type), '&', $.type))
    ]}
*)
and intersection_type = type_ option * type_ (* [type_ list]? *)

(** Literal Type

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
    ]}
  + JavaScript
    {@js[
     number: _ => {
       ...
       const bigintLiteral =
         seq(choice(hexLiteral,
                    binaryLiteral,
                    octalLiteral,
                    decimalDigits),
             'n'),

       return token(choice(
         hexLiteral,       // 0x12 0X12
         decimalLiteral,   // 12.5 10E2 .5 13
         binaryLiteral,    // 0b01 0B01
         octalLiteral,     // 0o12 0O12
         bigintLiteral))   // 12n 0x12n
     }
    ]}
*)
and literal_type =
  | T_unary_type of unary_type
  | T_number of number
  | T_string of string
  | T_true
  | T_false
  | T_null
  | T_undefined

and unary_type =
  { operator : sign
  ; argument : number
  }

and number =
  | Hex_literal of hex_literal
  | Dec_literal of dec_literal
  | Bin_literal of bin_literal
  | Oct_literal of oct_literal
  | Bigint_literal of bigint_literal

(** Lookup Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
     lookup_type: $ => seq($.primary_type, '[', $.type, ']')
    ]}
*)
and lookup_type = primary_type * type_

(** Object type

  The related grammar rules are given by:
  + TypeScript
    {@js[
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

     call_signature: $ => $._call_signature,

     construct_signature: $ => seq(
       optional('abstract'),
       'new',
       field('type_parameters', optional($.type_parameters)),
       field('parameters', $.formal_parameters),
       field('type', optional($.type_annotation)))
    ]}
*)
and object_type = member_type list

and member_type =
  | Export_statement of export_statement (* See STATEMENTS *)
  | Property_signature of property_signature
  | Call_signature of call_signature
  | Construct_signature of construct_signature
  | Index_signature of index_signature
  | Method_signature of method_signature

and property_signature =
  { access : accessibility_modifier option
  ; scope : method_scope
  ; name : property_name
  ; optional : bool
  ; type_ : type_annotation option
  }

and construct_signature =
  { abstract : bool
  ; type_parameters : type_parameters
  ; parameters : formal_parameters
  ; type_ : type_annotation option
  }

(** Predefined Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
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
    ]}
*)
and predefined_type =
  | T_any
  | T_number
  | T_string
  | T_symbol
  | T_unique_symbol
  | T_void
  | T_unknown
  | T_never
  | T_object

(** Template Literal Type

  The related grammar rules are given by:
  + TypeScript
    {@js[
     template_literal_type: $ => seq(
       '`',
       repeat(choice(
         alias($._template_chars, $.string_fragment),
         $.template_type)),
       '`'),

     // _template_chars???

     template_type: $ => seq('${', choice($.primary_type, $.infer_type), '}')
    ]}
*)
and template_literal_type = template_type list (* _template_chars? *)

and template_type =
  | Template_primary_type of primary_type
  | Template_infer_type of infer_type

(** Tuple Type

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
    ]}
*)
and tuple_type = tuple_type_member list

and tuple_type_member =
  | Tuple_parameter of tuple_parameter
  | Tuple_optional_parameter of optional_tuple_parameter
  | Tuple_optional_type of type_
  | Tuple_rest_type of type_
  | Type_type of type_

and tuple_parameter = tuple_parameter_name * type_annotation

and tuple_parameter_name =
  | Tuple_parameter_ident of identifier
  | Tuple_parameter_rest of rest_pattern

and optional_tuple_parameter = identifier * type_annotation

(** Type Query

  + TypeScript:
    {@js[
     type_query: $ => prec.right(seq(
       'typeof',
       choice(
         alias($._type_query_subscript_expression, $.subscript_expression),
         alias($._type_query_member_expression, $.member_expression),
         alias($._type_query_call_expression, $.call_expression),
         alias($._type_query_instantiation_expression,
               $.instantiation_expression),
         $.identifier,
         $.this))),

     _type_query_subscript_expression: $ => seq(
       field('object', choice(
         $.identifier,
         $.this,
         alias($._type_query_subscript_expression, $.subscript_expression),
         alias($._type_query_member_expression, $.member_expression),
         alias($._type_query_call_expression, $.call_expression))),
       optional('?.'),
       '[', field('index', choice($.predefined_type, $.string, $.number)), ']'),

     _type_query_member_expression: $ => seq(
       field('object', choice(
         $.identifier,
         $.this,
         alias($._type_query_subscript_expression, $.subscript_expression),
         alias($._type_query_member_expression, $.member_expression),
         alias($._type_query_call_expression, $.call_expression))),
       choice('.', '?.'),
       field('property', choice(
         $.private_property_identifier,
         alias($.identifier, $.property_identifier)))),

     _type_query_call_expression: $ => seq(
       field('function', choice(
         $.import,
         $.identifier,
         alias($._type_query_member_expression, $.member_expression),
         alias($._type_query_subscript_expression, $.subscript_expression))),
       field('arguments', $.arguments)),

     _type_query_instantiation_expression: $ => seq(
       field('function', choice(
         $.import,
         $.identifier,
         alias($._type_query_member_expression, $.member_expression),
         alias($._type_query_subscript_expression, $.subscript_expression))),
       field('type_arguments', $.type_arguments))
    ]}
*)
and type_query =
  | Typeof_subscript_expression of type_query_subscript_expression
  | Typeof_member_expression of type_query_member_expression
  | Typeof_call_expression of type_query_call_expression
  | Typeof_instantiation_expression of type_query_instantiation_expression
  | Typeof_identifier of identifier
  | Typeof_this

and type_query_subscript_expression =
  { object_ : type_query_object
  ; optional : bool
  ; index : type_query_index
  }

and type_query_object =
  | Type_query_object_identifier of identifier
  | Type_query_object_this
  | Type_query_object_subscript_expression of type_query_subscript_expression
  | Type_query_object_type_query_member_expression of type_query_member_expression
  | Type_query_object_call_expression of type_query_call_expression

and type_query_index =
  | Type_query_index_predefined_type of predefined_type
  | Type_query_index_string of string
  | Type_query_index_number of number

and type_query_member_expression =
  { object_ : type_query_object
  ; optional : bool
  ; property : type_query_property
  }

and type_query_property =
  | Type_query_property_private of private_property_identifier
  | Type_query_property_identifier of identifier

and type_query_call_expression =
  { function_ : type_query_call_function
  ; arguments : type_query_call_arguments
  }

and type_query_call_function =
  | Type_query_call_import of import
  | Type_query_call_identifier of identifier
  | Type_query_call_member_expresion of type_query_member_expression
  | Type_query_call_subscript_expression of type_query_subscript_expression

and type_query_call_arguments = arguments

and type_query_instantiation_expression =
  { function_ : type_query_call_function
  ; type_arguments : type_arguments
  }

(** Union Type

  Example:
  {@js[
   type t = A | B;
  ]}

  The related grammar rule is given by:
  + TypeScript
    {@js[
     union_type: $ => prec.left(seq(optional($.type), '|', $.type))
    ]}
*)
and union_type = type_ option * type_ (* [type_ list]? *)

(** Function Type

  Example:
  {@js[
   type t = <T>(x: T) => T;
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     function_type: $ => prec.left(seq(
       field('type_parameters', optional($.type_parameters)),
       field('parameters', $.formal_parameters),
       '=>',
       field('return_type', choice($.type, $.asserts, $.type_predicate)))),

     asserts: $ => seq(
       'asserts', choice($.type_predicate, $.identifier, $.this))
    ]}
*)
and function_type =
  { type_parameters : type_parameter option
  ; parameters : formal_parameters
  ; return_type : return_type
  }

and return_type =
  | Return_type of type_
  | Return_asserts of asserts
  | Return_type_predicate of type_predicate wrap

and asserts =
  | Assert_predicate of type_predicate wrap
  | Assert_type of identifier
  | Assert_this of keyword

(** Readonly Type

  The related grammar rules are given by:
  + TypeScript
    {@js[
     readonly_type: $ => seq('readonly', $.type)
    ]}
*)
and readonly_type = type_

(** Constructor Type

  The type of an object constructor.

  Example:
  {@js[
   type ctor = new <T>(x: T) => T;
  ]}

  The related grammar rules are given by:
  + TypeScript
    {@js[
     constructor_type: $ => prec.left(seq(
       optional('abstract'),
       'new',
       field('type_parameters', optional($.type_parameters)),
       field('parameters', $.formal_parameters),
       '=>',
       field('type', $.type)))
    ]}
*)
and constructor_type =
  { abstract : bool
  ; type_parameters : type_parameters
  ; parameters : formal_parameters
  ; type_ : type_
  }

(** Infer-type

  Example:
  {@js[
   type MyConditionalType<T> = T extends SomeType ? TrueType : FalseType;
   type MyInferredType<T> = T extends SomeType<infer U> ? U : FalseType;
  ]}

  The related grammar rule is given by:
  + TypeScript
    {@js[
     infer_type: $ => prec.right(seq(
       'infer',
       $._type_identifier,
       optional(seq('extends', $.type))))
    ]}
*)
and infer_type =
  { type_id : type_identifier
  ; extends : type_ option
  }

(** STATEMENTS

  The related grammar rule is given by:
  + JavaScript
    {@js[
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
    ]}
*)
and statement =
  | S_export_statement of export_statement
  | S_import_statement of import_statement
  | S_debugger_statement of keyword
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
  | S_empty_statement of Region.t
  | S_labeled_statement of labeled_statement

(** Break Statement

  The related grammar rule is given by:
  + JavaScript
    {@js[
     break_statement: $ => seq(
       'break',
       field('label', optional(alias($.identifier, $.statement_identifier))),
       $._semicolon)
    ]}
*)
and break_statement =
  { kwd_break : keyword
  ; stmt_id : identifier option
  }

(** Continue Statement

  The related grammar rule is given by:
  + JavaScript
    {@js[
     continue_statement: $ => seq(
       'continue',
       field('label', optional(alias($.identifier, $.statement_identifier))),
       $._semicolon)
    ]}
*)
and continue_statement =
  { kwd_continue : keyword
  ; stmt_id : identifier option
  }

(** Do-statement

  The related grammar rule is given by:
  + JavaScript
    {@js[
     do_statement: $ => prec.right(seq(
       'do',
       field('body', $.statement),
       'while',
       field('condition', $.parenthesized_expression),
       optional($._semicolon)))
    ]}
*)
and do_statement =
  { kwd_do : keyword
  ; body : statement
  ; kwd_while : keyword
  ; condition : expression
  }

(** Export Statement

  Examples:
  {@js[
   export {};
   export var pi = 3.14;
   export class C {};
   export function absolute(num: number) {
     if (num < 0) return num * -1;
     return num;
   }
  ]}

  The related grammar rules are given by:
  + JavaScript
    {@js[
     export_statement: $ => choice(
       seq('export',
           choice(
             seq('*', $._from_clause),
             seq($.namespace_export, $._from_clause),
             seq($.export_clause, $._from_clause), // or seq($.export_clause, optional($._from_clause))
             $.export_clause), // See above.
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
    ]}
  + TypeScript
    {@js[
     export_statement: ($, previous) => choice(
       previous,
       seq('export', 'type', $.export_clause,
           optional($._from_clause), $._semicolon),
       seq('export', '=', $.expression, $._semicolon),
       seq('export', 'as', 'namespace', $.identifier, $._semicolon))
    ]}
*)
and export_statement =
  | Export of export
  | Export_decorator of export_decorator
  | Export_type of export_clause * from_clause option
  | Export_equal of expression
  | Export_as_namespace of identifier

and export =
  | Export_from of from_clause
  | Export_as of namespace_export * from_clause
  | Export_clause of export_clause * from_clause option

and from_clause = string

and namespace_export =
  | Export_ident of identifier
  | Export_string of string

and export_clause = export_specifier list

and export_specifier =
  { name : module_export_name
  ; as_ : module_export_name option
  }

and module_export_name = namespace_export

and export_decorator =
  { decorators : decorators option
  ; export_dec : export_dec
  }

and export_dec =
  | Export_declaration of declaration
  | Export_default of export_default

and export_default =
  | Export_default_declaration of declaration
  | Export_default_expression of expression

(** Expression Statement

  Expressions can be used as statements, for example, a function call
  returning nothing (unit).

  The related grammar rule is given by:
  + TypeScript
    {@js[
     expression_statement: $ => seq($._expressions, $._semicolon)
    ]}
*)
and expression_statement = expressions

(** For-in/of Statement

  The related grammar rules are given by:
  + JavaScript
    {@js[
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
    ]}
*)
and for_in_statement =
  { await : keyword option
  ; kwd_for : keyword
  ; sym_lparen : symbol
  ; for_header : for_header
  ; syn_rparen : symbol
  ; body : statement
  }

and for_header =
  { range : for_range
  ; operator : for_operator
  ; collection : expressions
  }

and for_range =
  | For_in_expression of for_in_expression
  | For_in_variable of for_in_variable

and for_in_expression =
  | For_in_expression of lhs_expression
  | For_in_parenthesized of expression

and for_in_variable = for_in_kind * for_in_var

and for_in_kind =
  | Var
  | Let
  | Const

and for_in_var =
  | For_in_ident of identifier
  | For_in_pattern of destructuring_pattern

and for_operator =
  | In
  | Of

(** For-statement

  The related grammar rules are given by:
  + JavaScript
    {@js[
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
    ]}
*)
and for_statement =
  { kwd_for : keyword
  ; sym_lparen : symbol
  ; initializer_ : for_initializer
  ; condition : for_condition
  ; increment : expressions option
  ; sym_rparen : symbol
  ; body : statement
  }

and for_initializer =
  | For_lexical_declaration of lexical_declaration wrap
  | For_variable_declaration of variable_declaration
  | For_expression_statement of expression_statement
  | For_empty_statement of Region.t

and for_condition =
  | For_condition_expression of expression_statement
  | For_condition_empty of Region.t

(** If-statement

  The related grammar rules are given by:
  + JavaScript
    {@js[
     if_statement: $ => prec.right(seq(
       'if',
       field('condition', $.parenthesized_expression),
       field('consequence', $.statement),
       optional(field('alternative', $.else_clause)))),

     else_clause: $ => seq('else', $.statement)
   ]}
*)
and if_statement =
  { kwd_if : keyword
  ; condition : expression
  ; consequence : statement
  ; alternative : (keyword * statement) option (* "else" *)
  }

(** Import Statement

  Examples:
  {@js[
   import helloWorld from "./hello.js";
   import { pi, phi, absolute } from "./maths.js";
   import { pi as π } from "./maths.js";
   import RandomNumberGenerator, { pi as π } from "./maths.js";
   import { Cat, Dog } from "./animal.js";
   import * as math from "./maths.js";
   import type { Cat, Dog } from "./animal.js";
   import type { createCatName } from "./animal.js";
   import { createCatName, type Cat, type Dog } from "./animal.js";
   import fs = require("fs");
  ]}

  The related grammar rules are given by:
  + TypeScript:
    {@js[
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

     _import_identifier: $ =>
       choice($.identifier, alias('type', $.identifier)),

     _module_export_name: $ => choice($.identifier, $.string), // See exports

     import_require_clause: $ => seq(
       $.identifier, '=', 'require', '(', field('source', $.string), ')'),

     import_attribute: $ => seq(choice('with', 'assert'), $.object)
    ]}
*)
and import_statement =
  { import_kind : import_kind option
  ; import : import
  ; import_attribute : import_attribute option
  }

and import_kind =
  | Import_type
  | Import_typeof

and import =
  | Import_clause of import_clause * from_clause
  | Import_require_clause of import_require_clause
  | Import_source of string

and import_clause =
  | Import_namespace of namespace_import
  | Import_named of named_imports
  | Import_ident of import_identifier * namespace_or_named_import option

and namespace_or_named_import =
  | Import_namespace of namespace_import
  | Import_named of named_imports

and namespace_import = identifier (* "* as <ident>" *)
and named_imports = import_specifier list
and import_specifier = import_kind * import_specifier'

and import_specifier' =
  | Import_spec_name of import_identifier
  | Import_spec_alias of import_spec_alias

and import_identifier = identifier (* Including "type" *)

and import_spec_alias =
  { name : module_export_name
  ; alias : import_identifier
  }

and import_require_clause = identifier * string

and import_attribute =
  | Import_with of object_
  | Import_assert of object_

(** Asserts Annotation

  The related grammar rule is given by:
  + TypeScript
    {@js[
     asserts_annotation: $ => seq(seq(':', $.asserts)), // Really?
    ]}
*)
and asserts_annotation = asserts

(** Assignment Pattern

  The related grammar rule is given by:
  + JavaScript
    {@js[
     assignment_pattern: $ => seq(
       field('left', $.pattern), '=', field('right', $.expression))
    ]}
*)
and assignment_pattern =
  { left : pattern
  ; right : expression
  }

(** Try statement

  Try-with statements enable to guard a piece of code with exception
  handlers.

  The related grammar rules are given by:
  + JavaScript
    {@js[
     try_statement: $ => seq(
      'try',
      field('body', $.statement_block),
      optional(field('handler', $.catch_clause)),
      optional(field('finalizer', $.finally_clause))),

     finally_clause: $ => seq('finally', field('body', $.statement_block)),
    ]}
  + TypeScript
    {@js[
     catch_clause: $ => seq(
      'catch',
      optional(
        seq('(',
            field('parameter',
                  choice($.identifier, $._destructuring_pattern)),
            optional(field('type', $.type_annotation)),
            ')')),
      field('body', $.statement_block))
    ]}
*)
and try_statement =
  { kwd_try : keyword
  ; body : statement_block
  ; handler : catch_clause option
  ; finalizer : finally_clause option
  }

and catch_clause =
  { kwd_catch : keyword
  ; parameter : catch_parameter option
  ; body : statement_block
  }

and catch_parameter =
  { sym_lparen : symbol
  ; catch_parameter : catch_parameter_kind
  ; type_ : type_annotation option
  ; sym_rparen : symbol
  }

and catch_parameter_kind =
  | Catch_identifier of identifier
  | Catch_object_pattern of object_pattern
  | Catch_array_pattern of array_pattern

and finally_clause = statement_block

(** Class

  The related grammar rule is given by:
  + TypeScript
    {@js[
     class: $ => prec('literal', seq(
       repeat(field('decorator', $.decorator)),
       'class',
       field('name', optional($._type_identifier)),
       field('type_parameters', optional($.type_parameters)),
       optional($.class_heritage),
       field('body', $.class_body)))
    ]}
*)
and class_ =
  { decorators : decorators option
  ; name : type_identifier option
  ; type_parameters : type_parameters
  ; class_heritage : class_heritage
  ; body : class_member list
  }

(** DECORATOR

  The related grammar rules are given by:
  + TypeScript
    {@js[
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
    ]}
  + JavaScript
    {@js[
     decorator_member_expression: $ => prec('member', seq(
       field('object', choice(
         $.identifier,
         alias($.decorator_member_expression, $.member_expression))),
       '.',
       field('property', alias($.identifier, $.property_identifier))))
    ]}
*)
and decorator =
  | Decorator_identifier of identifier
  | Decorator_member_expression of decorator_member_expression wrap
  | Decorator_call_expression of decorator_call_expression wrap
  | Decorator_parenthesized_expression of decorator_parenthesized_expression

and decorator_member_expression =
  { object_ : object_member_expression
  ; property : identifier
  }

and object_member_expression =
  | Object_name of identifier
  | Qualified_member_expression of decorator_member_expression wrap

and decorator_call_expression =
  { function_ : function_or_property
  ; type_arguments : type_arguments option
  ; arguments : arguments
  }

and function_or_property =
  | Function_name of identifier
  | Qualified_member_expression of decorator_member_expression wrap

and decorator_parenthesized_expression =
  | Parenthesized_ident of identifier
  | Parenthesized_member of decorator_member_expression wrap
  | Parenthesized_call of decorator_call_expression wrap

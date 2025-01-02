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

(* Literals *)

type keyword = string wrap
type symbol = string wrap
type identifier = string wrap
type string_literal = string wrap
type hash_name = string wrap

(* Numbers

   Note: Only [dec_literal] used [Q.t]: the values for the other types
   are meant to be translated into Michelson bytes. *)

type hex_literal = (string * Hex.t) wrap
type bin_literal = (string * Hex.t) wrap
type oct_literal = (string * Hex.t) wrap
type dec_literal = (string * Q.t) wrap
type big = bool

type number =
  | Hex of hex_literal * big
  | Bin of bin_literal * big
  | Oct of oct_literal * big
  | Dec of dec_literal * big

(* Keywords

   TODO: Use unique data constructors for each keyword.
*)

type kwd_infer = keyword
type kwd_keyof = keyword
type kwd_meta = keyword
type kwd_target = keyword
type kwd_false = keyword
type kwd_true = keyword
type kwd_super = keyword
type kwd_null = keyword
type kwd_satisfies = keyword
type kwd_yield = keyword
type kwd_new = keyword
type kwd_instanceof = keyword
type kwd_implements = keyword
type kwd_assert = keyword
type kwd_as = keyword
type kwd_async = keyword
type kwd_function = keyword
type kwd_override = keyword
type kwd_readonly = keyword
type kwd_public = keyword
type kwd_private = keyword
type kwd_protected = keyword
type kwd_set = keyword
type kwd_get = keyword
type kwd_all = keyword
type kwd_static = keyword
type kwd_this = keyword
type kwd_is = keyword
type kwd_class = keyword
type kwd_const = keyword
type kwd_constraint = keyword
type kwd_let = keyword
type kwd_undefined = keyword
type kwd_abstract = keyword
type kwd_declare = keyword
type kwd_accessor = keyword
type kwd_global = keyword
type kwd_module = keyword
type kwd_enum = keyword
type kwd_import = keyword
type kwd_interface = keyword
type kwd_extends = keyword
type kwd_namespace = keyword
type kwd_type = keyword
type kwd_using = keyword
type kwd_return = keyword
type kwd_switch = keyword
type kwd_case = keyword
type kwd_default = keyword
type kwd_throw = keyword
type kwd_while = keyword
type kwd_with = keyword
type kwd_any = keyword
type kwd_number = keyword
type kwd_boolean = keyword
type kwd_string = keyword
type kwd_symbol = keyword
type kwd_unique_symbol = keyword
type kwd_void = keyword
type kwd_unknown = keyword
type kwd_never = keyword
type kwd_object = keyword
type kwd_asserts = keyword
type kwd_debugger = keyword
type kwd_break = keyword
type kwd_continue = keyword
type kwd_do = keyword
type kwd_export = keyword
type kwd_for = keyword
type kwd_await = keyword
type kwd_var = keyword
type kwd_in = keyword
type kwd_of = keyword
type kwd_if = keyword
type kwd_else = keyword
type kwd_typeof = keyword
type kwd_try = keyword
type kwd_catch = keyword
type kwd_require = keyword
type kwd_delete = keyword

(* Symbols

   TODO: Use unique data constructors for each symbol.
*)

type sym_arrow = symbol (* "=>" *)
type sym_lbracket = symbol (* "[" *)
type sym_rbracket = symbol (* "]" *)
type sym_ampersand = symbol (* "&" *)
type sym_vbar = symbol (* "|" *)
type sym_qmark = symbol (* "?" *)
type sym_equal = symbol (* "=" *)
type sym_colon = symbol (* ":" *)
type sym_star = symbol (* "*" *)
type sym_ellipsis = symbol (* "..." *)
type sym_plus = symbol (* "+" *)
type sym_minus = symbol (* "-" *)
type sym_bang = symbol (* "!" *)
type sym_dot = symbol (* "." *)
type sym_opt_chain = symbol (* "?." *)
type sym_lpar = symbol (* "(" *)
type sym_rpar = symbol (* ")" *)
type sym_tilde = symbol (* "~" *)
type sym_incr = symbol (* "++" *)
type sym_decr = symbol (* "--" *)
type sym_bquote = symbol (* "`" *)

(* Template string *)

type template_string_fragment =
  | String_fragment of string_literal
  | Escape_sequence of string_literal
  | Template_substitution of string wrap

type template_string = sym_bquote * template_string_fragment list * sym_bquote

(* Compound constructs *)

type 'a enclosed =
  { opening : symbol
  ; contents : 'a
  ; closing : symbol
  }

type 'a braces = Braces of 'a enclosed wrap
type 'a chevrons = Chevrons of 'a enclosed wrap
type 'a brackets = Brackets of 'a enclosed wrap
type 'a parens = Parens of 'a enclosed wrap

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
  | D_variable_declaration of variable_declaration wrap
  | D_function_signature of function_signature wrap
  | D_abstract_class_declaration of abstract_class_declaration wrap
  | D_module of module_declaration wrap
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
  { fun_sig : function_signature
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

     _semicolon: $ => choice($._automatic_semicolon, ';'),

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
  { kwd_async : kwd_async option
  ; kwd_function : kwd_function
  ; name : identifier
  ; call_sig : call_signature wrap
  }

and call_signature =
  { type_parameters : type_parameters option
  ; parameters : formal_parameters
  ; return_type : call_return_type option
  }

and type_parameters = type_parameter wrap list chevrons
and formal_parameters = formal_parameter wrap list parens

and formal_parameter =
  { parameter_name : parameter_name wrap
  ; optional : sym_qmark option
  ; type_opt : type_annotation option
  ; default : (sym_equal * expression) option
  }

and parameter_name =
  { decorators : decorators
  ; access : accessibility_modifier option
  ; kwd_override : kwd_override option
  ; kwd_readonly : kwd_readonly option
  ; pattern : parameter_pattern
  }

and decorators = decorator list

and parameter_pattern =
  | Parameter_pattern of pattern
  | Parameter_this of kwd_this

and call_return_type =
  | Type_annotation of type_annotation
  | Asserts_annotation of asserts_annotation
  | Type_predicate_annotation of type_predicate wrap

and type_annotation = sym_colon * type_expr

and type_predicate =
  { name : type_predicate_name
  ; kwd_is : kwd_is
  ; type_expr : type_expr
  }

and type_predicate_name =
  | Type_predicate_identifier of identifier
  | Type_predicate_this of kwd_this
  | Type_predicate_type of predefined_type

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
and generator_function_declaration = sym_star * function_declaration

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
  { decorators : decorators
  ; kwd_class : kwd_class
  ; name : type_identifier
  ; type_parameters : type_parameters option
  ; class_heritage : class_heritage option
  ; body : class_body
  }

and class_heritage =
  | Extends_clause of extends_clause * implements_clause option
  | Implements_clause of implements_clause

and extends_clause = kwd_extends * extends_clause_single wrap ne_list

and extends_clause_single =
  { value : expression
  ; type_arguments : type_arguments option
  }

and type_arguments = type_expr ne_list chevrons
and implements_clause = kwd_implements * type_expr ne_list

and type_parameter =
  { kwd_const : kwd_const option
  ; name : type_identifier
  ; constraint_expr : (kwd_constraint * type_expr) option
  ; default_type : (sym_equal * type_expr) option
  }

and class_body = class_member list braces

and class_member =
  | Method_definition of decorators * method_definition wrap
  | Method_signature of method_signature wrap
  | Call_static_block of (kwd_static * statement_block)
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
  ; scope : method_scope
  ; kwd_async : kwd_async option
  ; set_get_all : set_get_all option
  ; name : property_name
  ; optional : sym_qmark option
  ; call_sig : call_signature wrap
  }

and accessibility_modifier =
  | Public of kwd_public
  | Private of kwd_private
  | Protected of kwd_protected

and set_get_all =
  | Set of kwd_set
  | Get of kwd_get
  | All of sym_star

and method_scope =
  { kwd_static : kwd_static option
  ; kwd_override : kwd_override option
  ; kwd_readonly : kwd_readonly option
  }

and property_name =
  | Property_identifier of identifier (* Also reserved identifiers *)
  | Private_property_identifier of private_property_identifier
  | String of string_literal
  | Number of number
  | Computed_property_name of expression brackets

and private_property_identifier = hash_name

(** Lexical Declaration

  Lexical declarations are declarations of let- or
  const-variables. When achieved by means of object & tuple patterns,
  the variables they contain are introduced in the current scope.

  Example: {@js[const {x, y} = z;]}

  The related grammar rules are given by:
  + JavaScript
    {@js[
     lexical_declaration: $ => seq(
       field('kind', choice('let', 'const')),
       commaSep1($.variable_declarator),
       $._semicolon),

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
      variable_declarator: $ => choice(
        seq(
          field('name', choice($.identifier, $._destructuring_pattern)),
          field('type', optional($.type_annotation)),
          optional($._initializer),
        ),
        prec('declaration', seq(
          field('name', $.identifier),
          '!',
          field('type', $.type_annotation)))),

     _lhs_expression: ($, previous) => choice(previous, $.non_null_expression),

     non_null_expression: $ =>
       prec.left('unary', seq($.expression, '!'))
    ]}
 *)
and lexical_declaration =
  { kind : let_or_const
  ; decls : variable_declarator ne_list
  }

and let_or_const =
  | Let of kwd_let
  | Const of kwd_const

and variable_declarator =
  | Var_decl of var_decl_lhs wrap
  | Var_decl_assertion of var_decl_assertion

and var_decl_assertion = identifier * sym_qmark * type_annotation

and var_decl_lhs =
  { var_names : lhs_pattern
  ; var_type : type_annotation option
  ; default : (sym_equal * expression) option
  }

and lhs_pattern =
  | Decl_ident of identifier
  | Decl_pattern of destructuring_pattern

and destructuring_pattern =
  | Pattern_object of object_pattern
  | Pattern_array of array_pattern

and object_pattern = member_pattern list braces

and member_pattern =
  | Member_pair_pattern of pair_pattern
  | Member_rest_pattern of rest_pattern wrap
  | Member_object_assignment of object_assignment_pattern wrap
  | Member_shorthand_property of identifier (* Including reserved identifiers *)

and pair_pattern = (property_name, pair_value_pattern) key_value

and pair_value_pattern =
  | Pair_value of pattern
  | Pair_value_assignment of assignment_pattern wrap

and rest_pattern =
  { sym_ellipsis : sym_ellipsis
  ; expression : lhs_expression
  }

and lhs_expression =
  | Member_expression of member_expression wrap
  | Subscript_expression of subscript_expression wrap
  | Identifier of identifier (* Including reserved identifiers *)
  | Undefined of kwd_undefined
  | Pattern of destructuring_pattern
  | Non_null_expression of expression

and object_assignment_pattern =
  { left : object_lhs_pattern
  ; sym_equal : sym_equal
  ; right : expression
  }

and object_lhs_pattern = lhs_pattern
and array_pattern = array_cell_pattern list brackets

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
and variable_declaration = kwd_var * variable_declarator ne_list

(** Abstract Class Declaration

  An abstract class declaration is the declaration of a class that
  cannot be instantiated (no public constructors), and are instead
  used as a base to derive other classes, enforcing this way some
  method implementations and the presence of certain members with
  certain types.

  NOTE: We could have had the type [abstract_class_declaration] reuse
  the type [class_declaration], but we did not because of the
  different handling of comments (to which keyword they should be
  hooked, e.g., "class" or "abstract").

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
  { decorators : decorators
  ; kwd_abstract : kwd_abstract
  ; kwd_class : kwd_class
  ; name : type_identifier
  ; type_parameters : type_parameters option
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

and statement_block = statements braces

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
  ; kwd_abstract : kwd_abstract
  ; kwd_override : kwd_override option
  ; set_get_all : set_get_all option
  ; name : property_name
  ; optional : sym_qmark option
  ; call_sig : call_signature wrap
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

     type_annotation: $ => seq(':', $.type),
     omitting_type_annotation: $ => seq('-?:', $.type),
     adding_type_annotation: $ => seq('+?:', $.type),
     opting_type_annotation: $ => seq('?:', $.type)
    ]}
 *)
and index_signature =
  { sign : (sign option * kwd_readonly) option
  ; range : index_range brackets
  ; annotation : index_annotation
  }

and sign =
  | Plus of sym_plus
  | Minus of sym_minus

and index_range =
  | Typed_index_clause of typed_index_clause
  | Mapped_type_clause of mapped_type_clause

and typed_index_clause =
  { name : identifier (* Including reserved identifiers *)
  ; sym_colon : sym_colon
  ; index_type : type_expr
  }

and mapped_type_clause =
  { name : type_identifier
  ; kwd_in : kwd_in
  ; type_expr : type_expr
  ; alias : (kwd_as * type_expr) option
  }

and index_annotation =
  | Type_annotation of type_annotation
  | Omitting_type_annotation of (symbol * type_expr) (* "-?" *)
  | Adding_type_annotation of (symbol * type_expr) (* "+?:" *)
  | Opting_type_annotation of (symbol * type_expr)
(* "?: " *)

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
 *)
and public_field_definition =
  { decorators : decorators
  ; access : accessibility_modifier option
  ; kwd_declare : kwd_declare option
  ; scope : field_scope
  ; name : property_name
  ; mode : field_mode option
  ; type_ : type_annotation option
  ; default : (sym_equal * expression) option
  }

and field_scope =
  { kwd_static : kwd_static option
  ; kwd_override : kwd_override option
  ; kwd_readonly : kwd_readonly option
  ; kwd_abstract : kwd_abstract option
  ; kwd_accessor : kwd_accessor option
  }

and field_mode =
  | Optional of sym_qmark
  | Definite_assert of sym_bang

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
  { kwd_declare : kwd_declare
  ; ambient_kind : ambient_kind
  }

and ambient_kind =
  | Declaration of declaration
  | Global_declaration of kwd_global * statement_block
  | Module_declaration of
      kwd_module * identifier * type_expr (* "module", property identifier *)

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
       seq(field('name', $._property_name), $._initializer)
    }]
 *)
and enum_declaration =
  { kwd_const : kwd_const option
  ; kwd_enum : kwd_enum
  ; name : identifier
  ; body : enum_body list braces
  }

and enum_body =
  | Enum_name of property_name
  | Enum_assignment of enum_assignment wrap

and enum_assignment =
  { name : property_name
  ; default : sym_equal * expression
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
  { kwd_import : kwd_import
  ; alias : identifier
  ; sym_equal : sym_equal
  ; aliased : aliased
  }

and aliased =
  | Ident of identifier
  | Nested of nested_identifier wrap

(* The order is reversed in the path, e.g., A.B.c becomes [c; B; A] *)
and 'a nested = identifier ne_list * 'a
and nested_identifier = identifier nested (* property identifier *)

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
    }]
 *)
and interface_declaration =
  { kwd_interface : kwd_interface
  ; name : type_identifier
  ; type_parameters : type_parameters option
  ; extends : extends_type_clause option
  ; body : object_type (* See TYPES *)
  }

and extends_type_clause =
  { kwd_extends : kwd_extends
  ; extensions : type_extension ne_list
  }

and type_extension =
  | Extends_type of type_identifier
  | Extends_nested of nested_type_identifier wrap
  | Extends_generic of generic_type wrap

and nested_type_identifier = type_identifier nested

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
and internal_module =
  { kwd_namespace : kwd_namespace
  ; module_name : module_name
  ; module_body : statement_block option
  }

and module_declaration =
  { kwd_module : kwd_module
  ; module_name : module_name
  ; module_body : statement_block option
  }

and module_name =
  | Module_string of string_literal
  | Module_ident of identifier
  | Module_nested of nested_identifier wrap

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
  { kwd_type : kwd_type
  ; name : type_identifier
  ; type_parameters : type_parameters option
  ; sym_equal : sym_equal
  ; type_expr : type_expr
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
  | E_as_expression of as_expression wrap
  | E_assignment_expression of assignment_expression wrap
  | E_augmented_assignment_expression of augmented_assignment_expression wrap
  | E_await_expression of await_expression wrap
  | E_binary_expression of binary_expression wrap
  (*| E_glimmer_template of glimmer_template*)
  | E_instantiation_expression of instantiation_expression wrap
  | E_internal_module of internal_module wrap
  | E_new_expression of new_expression wrap
  | E_primary_expression of primary_expression
  | E_satisfies_expression of satisfies_expression wrap
  | E_ternary_expression of ternary_expression wrap
  | E_type_assertion of type_assertion wrap
  | E_unary_expression of unary_expression wrap
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
and as_expression = expression * kwd_as * as_what

and as_what =
  | As_type of type_expr
  | As_const of kwd_const

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
  { kwd_using : kwd_using option
  ; left : assignment_lhs
  ; sym_equal : sym_equal
  ; right : expression
  }

and assignment_lhs =
  | Assign_lhs_parens of parenthesized_expression
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
  { object_expr : object_member
  ; selector : selector
  ; property : property_ident
  }

and object_member =
  | Object_member_expression of expression
  | Object_member_import of kwd_import

and selector =
  | Dot of sym_dot
  | Optional_chain of sym_opt_chain

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
  { object_expr : expression
  ; optional_chain : optional_chain option
  ; index : expressions brackets
  }

and optional_chain = Optional_chain of sym_opt_chain
and expressions = sequence_expression
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
  | Member_expression of member_expression wrap
  | Subscript_expression of subscript_expression wrap
  | Identifier of identifier
  | Parenthesized_expression of parenthesized_expression

and assignment_operator =
  | Add_eq of symbol (* += *)
  | Sub_eq of symbol (* -= *)
  | Mult_eq of symbol (* *= *)
  | Div_eq of symbol (* /= *)
  | Rem_eq of symbol (* %= *)
  | Bit_xor_eq of symbol (* ^= *)
  | Bit_and_eq of symbol (* &= *)
  | Bit_or_eq of symbol (* |= *)
  | Bit_sr_eq of symbol (* >>= *)
  | Bit_usr_eq of symbol (* >>>= *)
  | Bit_sl_eq of symbol (* <<= *)
  | Exp_eq of symbol (* **= *)
  | Log_and_eq of symbol (* &&= *)
  | Log_or_eq of symbol (* ||= *)
  | Non_null_eq of symbol (* ??= *)

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
and await_expression =
  { kwd_await : kwd_await
  ; expression : expression
  }

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
  { lhs_expr : lhs_bin_expression
  ; operator : binary_operator
  ; rhs_expr : expression
  }

and lhs_bin_expression =
  | Lhs_bin_expression of expression
  | Lhs_bin_hash of private_property_identifier

and binary_operator =
  | Log_and of symbol (** && *)
  | Log_or of symbol (** || *)
  | Bit_sr of symbol (** >> *)
  | Bit_usr of symbol (** >>> *)
  | Bit_sl of symbol (** << *)
  | Bit_and of symbol (** &  *)
  | Bit_xor of symbol (** ^ *)
  | Bit_or of symbol (** | *)
  | Add of symbol (** + *)
  | Sub of symbol (** - *)
  | Mult of symbol (** * *)
  | Div of symbol (** / *)
  | Rem of symbol (** % *)
  | Exp of symbol (** ** *)
  | Lt of symbol (** < *)
  | Leq of symbol (** <= *)
  | Equal of symbol (** == *)
  | Strict_eq of symbol (** === *)
  | Neq of symbol (** != *)
  | Strict_neq of symbol (** !== *)
  | Geq of symbol (** >= *)
  | Gt of symbol (** > *)
  | Non_null of symbol (** ?? *)
  | Instance_of of kwd_instanceof (** instanceof *)
  | In of kwd_in (** in *)

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
  { kwd_new : kwd_new
  ; constructor : primary_expression
  ; type_arguments : type_arguments option
  ; arguments : arguments option
  }

and arguments = argument list parens

and argument =
  | Expression of expression
  | Spread_element of spread_element wrap

and spread_element = sym_ellipsis * expression

(** Satisfies-expression

  The 'satisfies' binary operator is like an `as` operator: it brings
  together an expression and a type. The difference is that the latter
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
and satisfies_expression = expression * kwd_satisfies * type_expr

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
  ; sym_qmark : sym_qmark
  ; consequence : expression
  ; sym_colon : sym_colon
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
  | Bang of sym_bang (* !x *)
  | Not of sym_tilde (* ~x *)
  | Unary_sub of sym_minus (* -x *)
  | Unary_add of sym_plus (* +x *)
  | Typeof of kwd_typeof (* typeof x *)
  | Void of kwd_void (* void x *)
  | Delete of kwd_delete (* delete x *)

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
  | Update_postfix of update wrap
  | Update_prefix of update wrap

and update =
  { argument : expression
  ; operator : incr_decr_operator
  }

and incr_decr_operator =
  | Increment of sym_incr
  | Decrement of sym_decr

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
  | Yield of (kwd_yield * expression option) wrap
  | Yield_iterable of (kwd_yield * sym_star * expression) wrap

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
  | E_arrow_function of arrow_function wrap
  | E_call_expression of call_expression
  | E_class of class_expression wrap
  | E_false of kwd_false
  | E_function_expression of function_expression wrap
  | E_generator_function of generator_function wrap
  | E_identifier of identifier
  | E_member_expression of member_expression wrap
  | E_meta_property of meta_property
  | E_non_null_expression of expression
  | E_null of kwd_null
  | E_number of number
  | E_object of object_expr
  | E_parenthesized_expression of parenthesized_expression
  | E_regex of string_literal
  | E_string of string_literal
  | E_subscript_expression of subscript_expression wrap
  | E_super of kwd_super
  | E_template_string of template_string wrap
  | E_this of kwd_this
  | E_true of kwd_true
  | E_undefined of kwd_undefined

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
and array = argument list brackets

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
  { kwd_async : kwd_async option
  ; parameters : parameters
  ; sym_arrow : sym_arrow
  ; body : function_body
  }

and parameters =
  | Parameter of identifier
  | Call_signature of call_signature wrap

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
  | Call of (fun_call, arguments_to_call) call wrap
  | Member of (primary_expression, arguments) call wrap

and fun_call =
  | Fun_call of expression
  | Import of kwd_import

and arguments_to_call =
  | Arguments of arguments
  | Template_string of template_string wrap

and ('lambda, 'arguments) call =
  { lambda : 'lambda
  ; type_arguments : type_arguments option
  ; arguments : 'arguments
  }

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
  { kwd_async : kwd_async option
  ; kwd_function : kwd_function
  ; name : identifier option
  ; call_sig : call_signature wrap
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
and generator_function = sym_star * function_expression wrap

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
  | Meta_new_target of (kwd_new * kwd_target) wrap
  | Meta_import_meta of (kwd_import * kwd_meta) wrap

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
and object_expr = object_entry list braces

and object_entry =
  | Object_entry_pair of pair
  | Object_entry_spread of spread_element wrap
  | Object_entry_method of method_definition wrap
  | Object_entry_shorthand of identifier

and pair = (property_name, expression) key_value

and ('key, 'value) key_value =
  { key : 'key
  ; sym_colon : sym_colon
  ; value : 'value
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
  ; sym_colon : sym_colon
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
  { kwd_return : kwd_return
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
  { kwd_switch : kwd_switch
  ; value : parenthesized_expression
  ; body : switch_body
  }

and switch_body = switch_entry list braces

and switch_entry =
  | Switch_case of switch_case wrap
  | Switch_default of switch_default wrap

and switch_case =
  { kwd_case : kwd_case
  ; value : expressions
  ; body : statement list
  }

and switch_default =
  { kwd_default : kwd_default
  ; statements : statement list
  }

(** Throw Statement

  The related grammar rule is given by:
  + JavaScript
    {@js[
     throw_statement: $ => seq('throw', $._expressions, $._semicolon)
    ]}
*)
and throw_statement =
  { kwd_throw : kwd_throw
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
  { kwd_while : kwd_while
  ; condition : parenthesized_expression
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
  { kwd_with : kwd_with
  ; object_expr : parenthesized_expression
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
  | P_member_expression of member_expression wrap
  | P_subscript_expression of subscript_expression wrap
  | P_identifier of identifier (* Including reserved identifiers *)
  | P_undefined of kwd_undefined
  | P_destructuring_pattern of destructuring_pattern
  | P_non_null_expression of expression
  | P_rest_pattern of rest_pattern wrap

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
and type_expr =
  | T_primary_type of primary_type
  | T_function_type of function_type wrap
  | T_readonly_type of readonly_type wrap
  | T_constructor_type of constructor_type wrap
  | T_infer_type of infer_type wrap
  | T_member_expression of type_query_member_expression_in_type_annotation wrap
  | T_call_expression of type_query_call_expression_in_type_annotation wrap

(** Type queries in type annotations (expressions)

  The related grammar rules ar given by:
  + TypeScript
    {@js[
      _type_query_member_expression_in_type_annotation: $ => seq(
        field('object', choice(
          $.import,
          alias($._type_query_member_expression_in_type_annotation, $.member_expression),
          alias($._type_query_call_expression_in_type_annotation, $.call_expression))),
        '.',
        field('property', choice(
          $.private_property_identifier,
          alias($.identifier, $.property_identifier)))),

      _type_query_call_expression_in_type_annotation: $ => seq(
        field('function', choice(
          $.import,
          alias($._type_query_member_expression_in_type_annotation, $.member_expression))),
        field('arguments', $.arguments))
    }]

    NOTE: The type expression "M.t" is parsed as a primary_type ->
    nested_type_identifier instead of a
    _type_query_member_expression_in_type_annotation. Looks like an
    ambiguity resolved elsewhere (either a "conflict" clause or
    perhaps order of definition). Likewise, "f(x)" does not parse as a
    type_query_call_expression_in_type_annotation, for some reason ("import(x)" does,
    though).
 *)
and type_query_member_expression_in_type_annotation =
  { object_expr : type_query_member_expression_object
  ; selector : sym_dot
  ; property : type_query_property
  }

and type_query_member_expression_object =
  | Type_query_object_import of kwd_import
  | Type_query_object_member of type_query_member_expression_in_type_annotation wrap
  | Type_query_object_call of type_query_call_expression_in_type_annotation wrap

and type_query_call_expression_in_type_annotation =
  { lambda : type_query_call_lambda
  ; arguments : arguments
  }

and type_query_call_lambda =
  | Type_query_call_import of kwd_import
  | Type_query_call_member of type_query_member_expression_in_type_annotation

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

     flow_maybe_type: $ => prec.right(seq('?', $.primary_type)),

     parenthesized_type: $ => seq('(', $.type, ')'),

     _type_identifier: $ => alias($.identifier, $.type_identifier),

     existential_type: _ => '*',

     index_type_query: $ => seq('keyof', $.primary_type)
    ]}
*)
and primary_type =
  | T_parenthesized_type of type_expr parens
  | T_predefined_type of predefined_type
  | T_type_identifier of type_identifier
  | T_nested_type_identifier of nested_type_identifier wrap
  | T_generic_type of generic_type wrap
  | T_object_type of object_type
  | T_array_type of array_type wrap
  | T_tuple_type of tuple_type
  | T_flow_maybe_type of (sym_qmark * primary_type) wrap
  | T_type_query of (kwd_keyof * type_query) wrap
  | T_index_type_query of (kwd_keyof * primary_type) wrap
  | T_this of kwd_this
  | T_existential_type of sym_star
  | T_literal_type of literal_type
  | T_lookup_type of lookup_type wrap
  | T_conditional_type of conditional_type wrap
  | T_template_literal_type of template_literal_type wrap
  | T_intersection_type of intersection_type wrap
  | T_union_type of union_type wrap

(** Generic type

  The related grammar rules are given by:
  + TypeScript
    {@js[
     generic_type: $ => prec('call', seq(
       field('name', choice(
         $._type_identifier,
         $.nested_type_identifier)),
       field('type_arguments', $.type_arguments)))
    }]

    *)
and generic_type = generic_name * type_arguments

and generic_name =
  | Generic_type of type_identifier
  | Generic_nested of nested_type_identifier wrap

(** Array Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
     array_type: $ => seq($.primary_type, '[', ']')
    ]}
*)
and array_type = primary_type * sym_lbracket * sym_rbracket

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
  { left : type_expr
  ; kwd_extends : kwd_extends
  ; right : type_expr
  ; sym_qmark : sym_qmark
  ; consequence : type_expr
  ; sym_colon : sym_colon
  ; alternative : type_expr
  }

(** Intersection Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
     intersection_type: $ => prec.left(seq(optional($.type), '&', $.type))
    ]}
  NOTE: [type_expr list] would have been better. *)
and intersection_type = type_expr option * sym_ampersand * type_expr

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
  | T_unary_type of unary_expression wrap
  | T_number of number
  | T_string of string_literal
  | T_true of kwd_true
  | T_false of kwd_false
  | T_null of kwd_null
  | T_undefined of kwd_undefined

and unary_type =
  { operator : sign
  ; argument : number
  }

(** Lookup Type

  The related grammar rule is given by:
  + TypeScript
    {@js[
     lookup_type: $ => seq($.primary_type, '[', $.type, ']')
    ]}
*)
and lookup_type = primary_type * type_expr brackets

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
and object_type = member_type list braces

and member_type =
  | Export_statement of export_statement wrap (* See STATEMENTS *)
  | Property_signature of property_signature
  | Call_signature of call_signature wrap
  | Construct_signature of construct_signature
  | Index_signature of index_signature
  | Method_signature of method_signature

and property_signature =
  { access : accessibility_modifier option
  ; scope : method_scope
  ; name : property_name
  ; sym_qmark : sym_qmark option
  ; type_ : type_annotation option
  }

and construct_signature =
  { kwd_abstract : kwd_abstract option
  ; kwd_new : kwd_new
  ; type_parameters : type_parameters option
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
  | T_any of kwd_any
  | T_number of kwd_number
  | T_boolean of kwd_boolean
  | T_string of kwd_string
  | T_symbol of kwd_symbol
  | T_unique_symbol of kwd_unique_symbol
  | T_void of kwd_void
  | T_unknown of kwd_unknown
  | T_never of kwd_never
  | T_object of kwd_object

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
and template_literal_type = sym_bquote * template_type_fragment list * sym_bquote

and template_type_fragment =
  | Template_type_string of string_literal
  | Template_type of template_type

and template_type =
  | Template_type_primary of primary_type
  | Template_type_infer of infer_type wrap

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
and tuple_type = tuple_type_member list brackets

and tuple_type_member =
  | Tuple_parameter of tuple_parameter wrap
  | Tuple_optional_parameter of optional_tuple_parameter wrap
  | Tuple_optional_type of (type_expr * sym_qmark) wrap
  | Tuple_rest_type of (sym_ellipsis * type_expr) wrap
  | Tuple_type of type_expr

and tuple_parameter = tuple_parameter_name * type_annotation

and tuple_parameter_name =
  | Tuple_parameter_ident of identifier
  | Tuple_parameter_rest of rest_pattern wrap

and optional_tuple_parameter = identifier * sym_qmark * type_annotation

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
  | Typeof_this of kwd_this

and type_query_subscript_expression =
  { object_expr : type_query_object
  ; optional : sym_opt_chain (* "?." *)
  ; index : type_query_index brackets
  }

and type_query_object =
  | Type_query_object_identifier of identifier
  | Type_query_object_this of kwd_this
  | Type_query_object_subscript_expression of type_query_subscript_expression
  | Type_query_object_member_expression of type_query_member_expression
  | Type_query_object_call_expression of type_query_call_expression

and type_query_index =
  | Type_query_index_predefined_type of predefined_type
  | Type_query_index_string of string_literal
  | Type_query_index_number of number

and type_query_member_expression =
  { object_expr : type_query_object
  ; selector : query_selector
  ; property : type_query_property
  }

and query_selector =
  | Query_selector_dot of sym_dot
  | Query_selector_opt_chain of sym_opt_chain

and type_query_property =
  | Type_query_property_private of private_property_identifier
  | Type_query_property_identifier of identifier

and type_query_call_expression =
  { lambda : type_query_call_function
  ; arguments : type_query_call_arguments
  }

and type_query_call_function =
  | Type_query_call_import of kwd_import
  | Type_query_call_identifier of identifier
  | Type_query_call_member_expression of type_query_member_expression
  | Type_query_call_subscript_expression of type_query_subscript_expression

and type_query_call_arguments = arguments

and type_query_instantiation_expression =
  { lambda : type_query_call_function
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

  NOTE:  [type_expr list] would have been better *)
and union_type = type_expr option * sym_vbar * type_expr

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
  { type_parameters : type_parameters option
  ; parameters : formal_parameters
  ; sym_arrow : sym_arrow
  ; return_type : return_type
  }

and return_type =
  | Return_type of type_expr
  | Return_asserts of asserts
  | Return_type_predicate of type_predicate wrap

and asserts =
  | Assert_predicate of kwd_asserts * type_predicate wrap
  | Assert_type of kwd_asserts * identifier
  | Assert_this of kwd_asserts * kwd_this

(** Readonly Type

  The related grammar rules are given by:
  + TypeScript
    {@js[
     readonly_type: $ => seq('readonly', $.type)
    ]}
*)
and readonly_type = kwd_readonly * type_expr

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
  { kwd_abstract : kwd_abstract option
  ; kwd_new : kwd_new
  ; type_parameters : type_parameters option
  ; parameters : formal_parameters
  ; sym_arrow : sym_arrow
  ; type_expr : type_expr
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
  { kwd_infer : kwd_infer
  ; type_id : type_identifier
  ; extends : (kwd_extends * type_expr) option
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
  | S_export_statement of export_statement wrap
  | S_import_statement of import_statement wrap
  | S_debugger_statement of kwd_debugger
  | S_expression_statement of expression_statement
  | S_declaration_statement of declaration
  | S_statement_block of statement_block
  | S_if_statement of if_statement wrap
  | S_switch_statement of switch_statement wrap
  | S_for_statement of for_statement wrap
  | S_for_in_statement of for_in_statement wrap
  | S_while_statement of while_statement wrap
  | S_do_statement of do_statement wrap
  | S_try_statement of try_statement wrap
  | S_with_statement of with_statement wrap
  | S_break_statement of break_statement wrap
  | S_continue_statement of continue_statement wrap
  | S_return_statement of return_statement wrap
  | S_throw_statement of throw_statement wrap
  | S_empty_statement of Region.t
  | S_labeled_statement of labeled_statement wrap

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
  { kwd_break : kwd_break
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
  { kwd_continue : kwd_continue
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
  { kwd_do : kwd_do
  ; body : statement
  ; kwd_while : kwd_while
  ; condition : parenthesized_expression
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
  { kwd_export : kwd_export
  ; export_kind : export_kind
  }

and export_kind =
  | Export_from of from_clause
  | Export_as of namespace_export * from_clause
  | Export_clause of export_clause * from_clause option
  | Export_declaration of declaration decorated
  | Export_default_declaration of (kwd_default * declaration) decorated
  | Export_default_expression of (kwd_default * expression) decorated
  | Export_type of export_type (* "type" *)
  | Export_equal of sym_equal * expression
  | Export_as_namespace of kwd_as * identifier

and from_clause = sym_star * string_literal

and namespace_export =
  { sym_star : sym_star
  ; kwd_as : kwd_as
  ; namespace_name : module_export_name
  }

and export_clause = export_specifier list braces

and export_specifier =
  { name : module_export_name
  ; alias : (kwd_as * module_export_name) option
  }

and module_export_name =
  | Export_ident of identifier
  | Export_string of string_literal

and export_type =
  { kwd_type : kwd_type
  ; export_clause : export_clause
  ; from_clause : from_clause option
  }

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
  { kwd_for : kwd_for
  ; kwd_await : kwd_await option
  ; sym_lpar : sym_lpar
  ; for_header : for_header
  ; sym_rpar : sym_rpar
  ; body : statement
  }

and for_header =
  { range : for_range
  ; operator : for_operator
  ; collection : expressions
  }

and for_range =
  | For_in_expression of lhs_expression
  | For_in_parenthesized of parenthesized_expression
  | For_in_var of for_in_var
  | For_in_let of kwd_let * for_in_variable
  | For_in_const of kwd_const * for_in_variable

and for_in_var =
  { kwd_var : kwd_var
  ; variable : for_in_variable
  ; default : expression option
  }

and for_in_variable =
  | For_in_ident of identifier
  | For_in_pattern of destructuring_pattern

and for_operator =
  | In of kwd_in
  | Of of kwd_of

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
  { kwd_for : kwd_for
  ; sym_lpar : sym_lpar
  ; initializer_ : for_initializer
  ; condition : for_condition
  ; increment : expressions option
  ; sym_rpar : sym_rpar
  ; body : statement
  }

and for_initializer =
  | For_lexical_declaration of lexical_declaration wrap
  | For_variable_declaration of variable_declaration wrap
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
  { kwd_if : kwd_if
  ; condition : parenthesized_expression
  ; consequence : statement
  ; alternative : (kwd_else * statement) option
  }

and parenthesized_expression = expressions parens

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
  { kwd_import : kwd_import
  ; import_kind : import_kind option
  ; import : import
  ; import_attribute : import_attribute option
  }

and import_kind =
  | Import_type of kwd_type
  | Import_typeof of kwd_typeof

and import =
  | Import_clause of import_clause * from_clause
  | Import_require_clause of import_require_clause wrap
  | Import_source of string_literal

and import_clause =
  | Import_namespace of namespace_import wrap
  | Import_named of named_imports
  | Import_ident of import_identifier * namespace_or_named_imports option

and namespace_or_named_imports =
  | Import_namespace of namespace_import wrap
  | Import_named of named_imports

and namespace_import =
  { sym_star : sym_star
  ; kwd_as : kwd_as
  ; identifier : identifier
  }

and named_imports = import_specifier list braces
and import_specifier = import_kind option * import_specifier'

and import_specifier' =
  | Import_spec_name of import_identifier
  | Import_spec_alias of import_spec_alias

and import_identifier = identifier (* Including "type" *)

and import_spec_alias =
  { name : module_export_name
  ; kwd_as : kwd_as
  ; alias : import_identifier
  }

and import_require_clause =
  { ident : identifier
  ; sym_equal : sym_equal
  ; kwd_require : kwd_require
  ; sym_lpar : sym_lpar
  ; source : string_literal
  ; sym_rpar : sym_rpar
  }

and import_attribute =
  | Import_with of kwd_with * object_expr
  | Import_assert of kwd_assert * object_expr

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
  ; sym_equal : sym_equal
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
  { kwd_try : kwd_try
  ; body : statement_block
  ; handler : catch_clause option
  ; finalizer : finally_clause option
  }

and catch_clause =
  { kwd_catch : kwd_catch
  ; parameter : catch_parameter option
  ; body : statement_block
  }

and catch_parameter =
  { sym_lpar : sym_lpar
  ; catch_parameter : catch_parameter_kind
  ; type_opt : type_annotation option
  ; sym_rpar : sym_rpar
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
and class_expression =
  { decorators : decorators
  ; kwd_class : kwd_class
  ; name : type_identifier option
  ; type_parameters : type_parameters option
  ; class_heritage : class_heritage option
  ; body : class_body
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
  | Decorator_parenthesized_expression of decorator_parenthesized_expression parens

and 'a decorated =
  { decorators : decorators
  ; decorated : 'a
  }

and decorator_member_expression =
  { object_expr : object_member_expression
  ; sym_dot : sym_dot
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

(* PROJECTIONS *)

(* Regions (a.k.a. source locations) *)

let region_of_braces (Braces w) = w#region
let region_of_chevrons (Chevrons w) = w#region
let region_of_brackets (Brackets w) = w#region
let region_of_parens (Parens w) = w#region

let region_of_call_expression = function
  | Call e -> e#region
  | Member e -> e#region

let region_of_meta_property = function
  | Meta_new_target e -> e#region
  | Meta_import_meta e -> e#region

let region_of_update_expression = function
  | Update_postfix e -> e#region
  | Update_prefix e -> e#region

let region_of_yield_expression = function
  | Yield e -> e#region
  | Yield_iterable e -> e#region

let region_of_number = function
  | Hex (e, _) -> e#region
  | Bin (e, _) -> e#region
  | Oct (e, _) -> e#region
  | Dec (e, _) -> e#region

let rec region_of_primary_expression = function
  | E_array e -> region_of_brackets e
  | E_arrow_function e -> e#region
  | E_call_expression e -> region_of_call_expression e
  | E_class e -> e#region
  | E_false e -> e#region
  | E_function_expression e -> e#region
  | E_generator_function e -> e#region
  | E_identifier e -> e#region
  | E_member_expression e -> e#region
  | E_meta_property e -> region_of_meta_property e
  | E_non_null_expression e -> region_of_expression e
  | E_null e -> e#region
  | E_number e -> region_of_number e
  | E_object e -> region_of_braces e
  | E_parenthesized_expression e -> region_of_parens e
  | E_regex e -> e#region
  | E_string e -> e#region
  | E_subscript_expression e -> e#region
  | E_super e -> e#region
  | E_template_string e -> e#region
  | E_this e -> e#region
  | E_true e -> e#region
  | E_undefined e -> e#region

and region_of_expression = function
  | E_as_expression e -> e#region
  | E_assignment_expression e -> e#region
  | E_augmented_assignment_expression e -> e#region
  | E_await_expression e -> e#region
  | E_binary_expression e -> e#region
  | E_instantiation_expression e -> e#region
  | E_internal_module e -> e#region
  | E_new_expression e -> e#region
  | E_primary_expression e -> region_of_primary_expression e
  | E_satisfies_expression e -> e#region
  | E_ternary_expression e -> e#region
  | E_type_assertion e -> e#region
  | E_unary_expression e -> e#region
  | E_update_expression e -> region_of_update_expression e
  | E_yield_expression e -> region_of_yield_expression e

let region_of_destructuring_pattern = function
  | Pattern_object (Braces b) -> b#region
  | Pattern_array (Brackets b) -> b#region

let region_of_pattern = function
  | P_member_expression p -> p#region
  | P_subscript_expression e -> e#region
  | P_identifier p -> p#region
  | P_undefined p -> p#region
  | P_destructuring_pattern p -> region_of_destructuring_pattern p
  | P_non_null_expression e -> region_of_expression e
  | P_rest_pattern p -> p#region

let region_of_declaration = function
  | D_function_declaration d -> d#region
  | D_generator_function_declaration d -> d#region
  | D_class_declaration d -> d#region
  | D_lexical_declaration d -> d#region
  | D_variable_declaration d -> d#region
  | D_function_signature d -> d#region
  | D_abstract_class_declaration d -> d#region
  | D_module d -> d#region
  | D_internal_module d -> d#region
  | D_type_alias_declaration d -> d#region
  | D_enum_declaration d -> d#region
  | D_interface_declaration d -> d#region
  | D_import_alias d -> d#region
  | D_ambient_declaration d -> d#region

let region_of_statement = function
  | S_export_statement s -> s#region
  | S_import_statement s -> s#region
  | S_debugger_statement s -> s#region
  | S_expression_statement s -> s#region
  | S_declaration_statement d -> region_of_declaration d
  | S_statement_block (Braces s) -> s#region
  | S_if_statement s -> s#region
  | S_switch_statement s -> s#region
  | S_for_statement s -> s#region
  | S_for_in_statement s -> s#region
  | S_while_statement s -> s#region
  | S_do_statement s -> s#region
  | S_try_statement s -> s#region
  | S_with_statement s -> s#region
  | S_break_statement s -> s#region
  | S_continue_statement s -> s#region
  | S_return_statement s -> s#region
  | S_throw_statement s -> s#region
  | S_empty_statement r -> r
  | S_labeled_statement s -> s#region

let region_of_decorator = function
  | Decorator_identifier d -> d#region
  | Decorator_member_expression d -> d#region
  | Decorator_call_expression d -> d#region
  | Decorator_parenthesized_expression (Parens d) -> d#region

let region_of_predefined_type = function
  | T_any t -> t#region
  | T_number t -> t#region
  | T_boolean t -> t#region
  | T_string t -> t#region
  | T_symbol t -> t#region
  | T_unique_symbol t -> t#region
  | T_void t -> t#region
  | T_unknown t -> t#region
  | T_never t -> t#region
  | T_object t -> t#region

let region_of_literal_type = function
  | T_unary_type t -> t#region
  | T_number n -> region_of_number n
  | T_string t -> t#region
  | T_true t -> t#region
  | T_false t -> t#region
  | T_null t -> t#region
  | T_undefined t -> t#region

let region_of_primary_type = function
  | T_parenthesized_type (Parens t) -> t#region
  | T_predefined_type t -> region_of_predefined_type t
  | T_type_identifier t -> t#region
  | T_nested_type_identifier t -> t#region
  | T_generic_type t -> t#region
  | T_object_type (Braces t) -> t#region
  | T_array_type t -> t#region
  | T_tuple_type (Brackets t) -> t#region
  | T_flow_maybe_type t -> t#region
  | T_type_query t -> t#region
  | T_index_type_query t -> t#region
  | T_this t -> t#region
  | T_existential_type t -> t#region
  | T_literal_type t -> region_of_literal_type t
  | T_lookup_type t -> t#region
  | T_conditional_type t -> t#region
  | T_template_literal_type t -> t#region
  | T_intersection_type t -> t#region
  | T_union_type t -> t#region

let region_of_type_expr = function
  | T_primary_type t -> region_of_primary_type t
  | T_function_type t -> t#region
  | T_readonly_type t -> t#region
  | T_constructor_type t -> t#region
  | T_infer_type t -> t#region
  | T_member_expression t -> t#region
  | T_call_expression t -> t#region

let region_of_template_type = function
  | Template_type_primary t -> region_of_primary_type t
  | Template_type_infer t -> t#region

let region_of_template_type_fragment = function
  | Template_type_string s -> s#region
  | Template_type t -> region_of_template_type t

let region_of_tuple_type_member = function
  | Tuple_parameter w -> w#region
  | Tuple_optional_parameter w -> w#region
  | Tuple_optional_type w -> w#region
  | Tuple_rest_type w -> w#region
  | Tuple_type type_expr -> region_of_type_expr type_expr

let region_of_asserts = function
  | Assert_predicate (kwd_asserts, type_predicate) ->
    Region.cover kwd_asserts#region type_predicate#region
  | Assert_type (kwd_asserts, identifier) ->
    Region.cover kwd_asserts#region identifier#region
  | Assert_this (kwd_asserts, kwd_this) -> Region.cover kwd_asserts#region kwd_this#region

let region_of_return_type = function
  | Return_type t -> region_of_type_expr t
  | Return_asserts a -> region_of_asserts a
  | Return_type_predicate w -> w#region

let region_of_accessibility_modifier = function
  | Public kwd_public -> kwd_public#region
  | Private kwd_private -> kwd_private#region
  | Protected kwd_protected -> kwd_protected#region

let region_of_lhs_expression = function
  | Member_expression w -> w#region
  | Subscript_expression w -> w#region
  | Identifier var -> var#region
  | Undefined kwd_undefined -> kwd_undefined#region
  | Pattern p -> region_of_destructuring_pattern p
  | Non_null_expression e -> region_of_expression e

let region_of_for_in_variable = function
  | For_in_ident ident -> ident#region
  | For_in_pattern p -> region_of_destructuring_pattern p

let region_of_module_name = function
  | Module_string literal -> literal#region
  | Module_ident ident -> ident#region
  | Module_nested nested -> nested#region

let region_of_lhs_pattern = function
  | Decl_ident id -> id#region
  | Decl_pattern p -> region_of_destructuring_pattern p

let region_of_array_cell_pattern = function
  | Cell_pattern pattern -> region_of_pattern pattern
  | Cell_assignment pattern -> pattern#region

let region_of_augmented_assignment_lhs (node : augmented_assignment_lhs) =
  match node with
  | Member_expression w -> w#region
  | Subscript_expression w -> w#region
  | Identifier ident -> ident#region
  | Parenthesized_expression expr -> region_of_parens expr

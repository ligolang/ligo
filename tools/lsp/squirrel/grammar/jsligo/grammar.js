// const PREC = {
//   OR: 0,
//   AND: 1,
//   COMPARE: 3,
//   CONCAT: 5,
//   PLUS: 6,
//   MINUS: 6,
//   MUL: 7,
//   SHIFT: 8,
//   TYPE: 101,
//   LET: 100,
// };

// const OPS = [
//   ['+', PREC.PLUS],
//   ['-', PREC.MINUS],
//   ['%', PREC.MUL],
//   ['/', PREC.MUL],
//   ['*', PREC.MUL],
//   // ['&', PREC.MUL],
//   // ['|', PREC.MUL],
//   // ['^', PREC.MUL],
//   // ['<<', PREC.SHIFT],
//   // ['>>', PREC.SHIFT],
//   // ['++', PREC.CONCAT],
//   ['<', PREC.COMPARE],
//   ['>', PREC.COMPARE],
//   ['<=', PREC.COMPARE],
//   ['>=', PREC.COMPARE],
//   ['==', PREC.COMPARE],
//   ['!=', PREC.COMPARE],
//   ['&&', PREC.COMPARE],
//   ['||', PREC.OR],
// ]

const { sepBy } = require('../common.js');
const common = require('../common.js')

module.exports = grammar({
  name: 'JsLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, /\s/],

  // This grammar is not LR(1), thus it has a bunch of conflicts.
  // There are two major non-LR(1)-ities.
  // The first one is Expr/Pattern conflict at the beginning of expression.
  // For example, in ((x . , ... x might be an expression in case of ((x, y))
  // and x might be a pattern in case of ((x, y) => x * y).
  // The second one is Expr/Type conflict in lambda type annotation.
  // For example, in (x) : y => z . ( z might be an expression in case of (x) : y => z (w)
  // and z might be a type in case of (x) : y => z (w) => v where
  // y => z (w) is lambda type annotation and v is lambda body.
  // Those major conflicts lead to a bunch of conflicts in grammar.
  // For example, [] might be an expression or a pattern.
  // In case of expression, the parser follows list rule.
  // In case of pattern, the parser follows list_pattern rule.
  // As a result we get list/list_pattern conflict.
  // In {x, ... x might be either FieldName in record expression or
  // NameDecl in record pattern, so we get FieldName/NameDecl conflict.
  // It is possible to combine several conflicts into one
  // by combining conflicting rules together, but it leads to problems in Reasonligo.hs.
  // Also, there is a combination of two major conflicts, for example for z in (x) : y => (z . )

  // conflicts: $ =>
  //   [ // Pattern/Expr conflicts
  //     [$._expr_term, $._unannotated_pattern]
  //   , [$._pattern, $.fun_arg]
  //   , [$.FieldName, $.NameDecl]
  //   , [$.Name, $.NameDecl]
  //   , [$.list, $.list_pattern]

  //     // Type/Expr conflicts
  //   , [$.Name, $.TypeName]
  //   , [$.FieldName, $.TypeName]
  //   , [$.string_type, $._literal]
  //   , [$.TypeWildcard, $.wildcard]
  //   , [$._core_type, $._literal]

  //     // Pattern/Expr + Type/Expr conflicts
  //   , [$.Name, $.NameDecl, $.TypeName]
  //   , [$.NameDecl, $.TypeName]
  //   ],

  rules: {
    source_file: $ => choice(
      common.sepEndBy(';', field("toplevel", $.statement_or_namespace)),
      seq(field("toplevel", statement_or_namespace), optional(";"))),

    statement_or_namespace: $ => choice($.statement, $.namespace_statement),

    namespace_statement: $ => seq(optional("export"), $.namespace),

    namespace: $ => seq("namespace", field("moduleName", $.ModuleName), common.block($.statements_or_namespace)),

    statements_or_namespace: $ => sepBy(";", statement_or_namespace),

    statement: $ => choice($.base_statement, $.if_statement),

    if_statement: $ => seq("if", common.par($.expr), $.statement),

    base_statement: $ => choice(
      $.expr_statement,
      $.return_statement,
      $.block_statement,
      $.switch_statement,
      $.import_statement,
      $.export_statement,
      $.declaration_statement,
      $.if_else_statement,
      $.for_of_statement,
      $.while_statement,
    ),

    expr_statement: $ => choice(
      seq($.assignment_expr_level, "=", $.expr_statement),
      seq($.assignment_expr_level, "*=", $.expr_statement),
      seq($.assignment_expr_level, "/=", $.expr_statement),
      seq($.assignment_expr_level, "%=", $.expr_statement),
      seq($.assignment_expr_level, "+=", $.expr_statement),
      seq($.assignment_expr_level, "-=", $.expr_statement),
      $.fun_expr,
      $.assignment_expr_level
    ),

    assignment_expr_level: $ => choice(
      seq($.assignment_expr_level, "as", $.type_expr),
      $.disjunction_expr_level
    ),

    disjunction_expr_level: $ => choice(
      seq($.disjunction_expr_level, "||", conjunction_expr_level),
      $.conjunction_expr_level
    ),

    conjunction_expr_level: $ => choice(
      seq($.conjunction_expr_level, "&&", $.comparison_exp_level),
      $.comparison_expr_level
    ),

    comparison_expr_level: $ => choice(
      seq($.comparison_expr_level, "<", $.addition_expr_level),
      seq($.comparison_expr_level, "<=", $.addition_expr_level),
      seq($.comparison_expr_level, ">", $.addition_expr_level),
      seq($.comparison_expr_level, ">=", $.addition_expr_level),
      seq($.comparison_expr_level, "==", $.addition_expr_level),
      seq($.comparison_expr_level, "!=", $.addition_expr_level),
      $.addition_expr_level
    ),

    addition_expr_level: $ => choice(
      seq($.addition_expr_level, "+", $.multiplication_expr_level),
      seq($.addition_expr_level, "-", $.multiplication_expr_level),
      $.multiplication_expr_level
    ),

    multiplication_expr_level: $ => choice(
      seq($.multiplication_expr_level, "*", $.unary_expr_level),
      seq($.multiplication_expr_level, "/", $.unary_expr_level),
      seq($.multiplication_expr_level, "%", $.unary_expr_level),
      $.unary_expr_level
    ),

    unary_expr_level: $ => choice(
      seq("-", $.call_expr_level),
      seq("!", $.call_expr_level),
      $.call_expr_level
    ),

    call_expr_level: $ => choice($.call_expr, $.member_expr),

    call_expr: $ => seq($.lambda, common.par(optional(sepBy(",", $.expr)))),

    lambda: $ => choice($.call_expr, $.member_expr),

    expr: $ => choice($.expr_statement, $.object_literal),

    member_expr: $ => choice(
      $.Name,
      $.Int,
      $.Bytes,
      $.String,
      $.ctor_expr,
      $.projection,
      $.michelson_interop,
      common.par($.expr),
      $.module_access,
      $.array_literal,
      $.wildcard
    ),

    ctor_expr: $ => seq($.ConstrName, common.par(optional($.ctor_args))),

    ctor_args: $ => sepBy(",", $.expr),

    projection: $ => choice(
      seq($.member_expr, common.brackets($.expr)),
      seq($.member_expr, ".", $.Name)
    ),

    michelson_interop: $ => seq(
      '(Michelson',
        seq(
          field("code", $.michelson_code),
          'as',
          field("type", $._type_expr),
        ),
      ')'
    ),

    michelson_code: $ => seq('`', repeat(/([^\|]|\|[^}])/), '`'), // check ???

    module_access: $ => seq($.ModuleName, ".", $.module_var),

    module_var: $ => choice($.module_access, $.Name),

    array_literal: $ => common.brackets(optional(sepBy(",", $.array_item))),

    array_item: $ => choice($.expr, seq("...", $.expr)), 

    fun_expr: $ => choice(
      seq(common.par($.parameters), optional($.type_annotation), "=>", $.body),
      seq("(", ")", optional($.type_annotation), "=>", $.body),
      seq($.Name, "=>", $.body)
    ),

    body: $ => choice(common.block($.statements), $.expr_stmt),

    statements: $ => sepBy(";", $.statement),

    type_annotation: $ => seq(":", $.type_expr),

    parameters: $ => sepBy(",", $.parameter),

    parameter: $ => seq($.expr, $.type_annotation),

    type_expr: $ => ,

    return_statement: $ => choice("return", seq("return", $.expr)),

    block_statement: $ => common.block($.statements),

    object_literal: $ => common.block(optional(sepBy(",", $.property))),

    property: $ => choice($.Name, seq($.property_name, ":", $.expr), seq("...", $.expr_statement)),

    property_name: $ => choice($.Int, $.String, $.ConstrName, $.Name),

    




















    source_file: $ => common.sepEndBy(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $.type_decl,
        $.let_decl,
        $.preprocessor,
        $.module_decl,
        $.module_alias
       ),

    /// TYPE DECLARATIONS

    type_decl: $ =>
      seq(
        optional($.Export),
        'type',
        field("type_name", $.TypeName),
        optional(field("params", $.type_params)),
        '=',
        field("type_value", $._type_expr),
      ),

    type_params: $ => common.chev(common.sepBy1(",", field("param", $.var_type))),

    _type_expr: $ =>
      choice(
        $.TypeWildcard,
        $.fun_type,
        $.param_type,
        $.sum_type,
        $.record_type,
        $.tuple_type,
        $.string_type,
        $._core_type,
      ),

    sum_type: $ => choice(
      common.sepBy1('|', field("variant", $.variant)),
      common.withAttrs($, seq('|', common.sepBy1('|', field("variant", $.variant)))),
    ),

    variant: $ =>
      prec.left(8,
        common.withAttrs($, seq(
          '[',
          field("constructor", $.ConstrNameDecl),
          optional(
            seq(',', field("arguments", $._type_expr))),
          ']',
        )),
      ),

    record_type: $ => common.withAttrs($,
      common.block(common.sepEndBy1(',', field("field", $.field_decl)))
    ),

    field_decl: $ =>
      prec(10, // see 'accessor_chain' for explanation of precedence
        common.withAttrs($, seq(
          field("field_name", $.FieldName),
          ':',
          field("field_type", $._type_expr),
        ))),

    fun_type: $ =>
      prec.right(8, // ???
        seq(
          '(',
          common.sepBy(',', field("domain", $.param_type)),
          ')',
          '=>',
          field("codomain", $._type_expr),
        ),
      ),
    
    param_type: $ => seq(field("parameter", $.var_pattern), ':', field("parameter_type", $._type_expr)),

    _core_type: $ =>
      choice(
        $.Int,
        $.TypeName,
        $.app_type,
        $.module_TypeName,
        $.var_type,
      ),

    var_type: $ => seq(
      "'",
      field("name", $.TypeVariableName),
    ),

    app_type: $ =>
      seq(
        field("functor", $._core_type),
        field("arguments", $._type_arguments),
      ),

    _type_arguments: $ => common.chev(common.sepBy(',', field("argument", $._type_expr))),

    module_TypeName: $ =>
      seq(
        common.sepBy1('.', field("path", $.ModuleName)),
        '.',
        field("type", $.TypeName),
      ),

    tuple_type: $ =>
      common.brackets(common.sepBy1(',', field("element", $._type_expr))),

    string_type: $ => field("value", $.String),

    /// LET DECLARATIONS

    let_decl: $ => prec.left(PREC.LET, common.withAttrs($, seq(
      optional($.Export),
      choice('let', 'const'),
      field("binding", $._pattern),
      optional(seq(
        ':',
        field("type", $._type_expr)
      )),
      '=',
      field("value", $._expr),
      optional(seq('as', field("type", $._type_expr)))
    ))),

    /// MODULES

    module_decl: $ => seq(
      optional($.Export),
      "namespace",
      field("moduleName", $.ModuleName),
      "{",
      common.sepEndBy(optional(';'), field("declaration", $._declaration)),
      "}"
    ),

    module_alias: $ => seq(
      optional($.Export),
      "import",
      field("moduleName", $.ModuleName),
      "=",
      common.sepBy('.', field("module", $.ModuleName))
    ),

    /// STATEMENTS

    _statement: $ => prec(1, choice(
      $.let_decl,
      $.type_decl,
      $._expr,
      $.module_decl,
      $.module_alias,
    )),

    /// PATTERNS

    _pattern: $ =>
      choice(
        $._unannotated_pattern,
        $.tuple_pattern,
      ),

    _unannotated_pattern: $ =>
      choice(
        $.wildcard,
        $._literal,
        $.paren_pattern,
        $.var_pattern,
        $.constr_pattern,
        $.list_pattern,
        $.record_pattern,
      ),

    _closed_pattern: $ => choice(
      $._pattern,
      $.annot_pattern,
    ),

    _literal: $ =>
      choice(
        $.Nat,
        $.Int,
        $.Bool,
        $.Tez,
        $.String,
        $.Bytes,
        $.Unit,
        $.None,
      ),

    tuple_pattern: $ => prec.left(13, seq(
      field("pattern", $._pattern),
      ',',
      common.sepBy1(',', field("pattern", $._pattern)),
    )),

    paren_pattern: $ => common.par(
      field("pattern", $._closed_pattern),
    ),

    var_pattern: $ => field("var", $.NameDecl),

    annot_pattern: $ => seq(
      field("subject", $._pattern),
      ':',
      field("type", $._type_expr),
    ),

    constr_pattern: $ => prec(1, seq(
      field("constructor", $.ConstrName),
      optional(field("arg", $._unannotated_pattern)),
    )),

    list_pattern: $ => common.brackets(
      common.sepEndBy(',', field("pattern", $._spread_pattern)),
    ),

    _spread_pattern: $ => choice(
      $.spread_pattern,
      $._unannotated_pattern,
    ),

    spread_pattern: $ => seq(
      '...',
      field("expr", $._unannotated_pattern),
    ),

    record_pattern: $ => common.withAttrs($, common.block(
      common.sepEndBy1(",", field("field", $._record_field_pattern)),
    )),

    _record_field_pattern: $ => choice(
      $.record_field_pattern,
      $.record_capture_pattern,
    ),

    record_field_pattern: $ => common.withAttrs($, prec(9, seq(
      field("name", $.FieldName),
      ":",
      field("body", $._unannotated_pattern),
    ))),

    record_capture_pattern: $ => common.withAttrs($, prec(9, field("name", $.NameDecl))),

    /// PROGRAM

    _program: $ => prec(1, choice(
      // $.let_in,
      $._expr
    )),

    // let_in: $ => seq(
    //   field("declaration", $._declaration),
    //   optional(seq('as', field("type", $._type_expr))),
    //   ';',
    //   // field("body", $._program),
    // ),

    /// EXPRESSIONS

    _expr: $ => choice(
      $.lambda,
      $.indexing,
      $.binary_call,
      $.unary_call,
      $.apply,
      $.Some_call,
      $.module_access,
      $._expr_term,
    ),

    fun_arg: $ => seq(
      field("argument", $._unannotated_pattern),
      seq(
        ':',
        field("type", $._type_expr),
      ),
    ),

    lambda: $ => prec.right(12, seq(
      common.par(common.sepBy(',', field("argument", $.fun_arg))),
      optional(seq(
        ':',
        field("type", $._type_expr),
      )),
      '=>',
      field("body", $._program),
    )),

    indexing: $ => prec.right(12, seq(
      field("box", $._expr),
      common.brackets(
        field("index", $.PositiveInt),
      )
    )),

    binary_call: $ => choice(
      ...OPS
        .map(([op, precendence]) =>
          prec.left(precendence, seq(
            field("left", $._expr),
            field("op", $[op]),
            field("right", $._expr),
          ))
        )
    ),

    unary_call: $ => prec.right(8, seq(field("negate", $.negate), field("arg", $._expr_term))),

    negate: $ => choice('-', '!'),

    // Workaround to make operators to be statements as well
    // so that they won't be stripped when naming them as fields
    ...OPS.reduce(
      (acc, [e, _]) => ({ ...acc, [e]: $ => seq(e) }), {}
    ),

    apply: $ => prec.left(20, seq(
      field("function", $._expr),
      common.par(common.sepBy(',', field("argument", $._program))),
    )),

    // ???
    Some_call: $ => prec.right(10, seq(
      field("some", $.Some),
      field("argument", $._expr),
    )),

    module_access: $ => seq(
      common.sepBy1('.', field("path", $.ModuleName)),
      '.',
      field("field", $.FieldName),
    ),

    _expr_term: $ => choice(
      $.ConstrName,
      $.Name,
      $._literal,
      // $.block,
      $.tuple,
      $.list,
      $.data_projection,
      // $.if,
      // $.switch,
      $._record_expr, // check ???
      $.michelson_interop,
      $.paren_expr,
      // $.let_in,
    ),

    tuple: $ => common.brackets(seq(
      field("item", $._annot_expr),
      ',',
      common.sepBy1(',', field("item", $._annot_expr)),
    )),

    _annot_expr: $ => choice(
      $.annot_expr,
      $._expr,
    ),

    annot_expr: $ => seq(
      field("subject", $._expr),
      'as',
      field("type", $._type_expr),
    ),

    list: $ => seq("list([",
      common.sepEndBy(',', field("element", $._spread_expr)),
      "])"
    ),

    _spread_expr: $ => choice(
      $._expr,
      $.spread,
    ),

    spread: $ => seq(
      '...',
      field("name", $._expr),
    ),

    // a.attribute
    data_projection: $ => seq(
      field("expr", $.Name),
      '.',
      $._accessor_chain,
    ),

    // The precedence is chosen so as to overtake over
    // 'field_decl'. The contract where it is relevant is
    // 'tools/lsp/squirrel/test/contracts/sexps/single_record_item.religo'.
    _accessor_chain: $ => prec.right(11, common.sepBy1('.', field("accessor", $.FieldName))),

    if: $ => seq(
      'if',
      common.par(field("selector", $._expr)),
      field("then", $.block),
      optional(seq(
        'else',
        field('else', $.block),
      ))
    ),

    switch: $ => seq(
      'switch',
      common.par(field("subject", $._expr_term)),
      common.block(
        seq(
          optional('|'),
          common.sepBy('|', field("alt", $.alt)),
        )
      )
    ),

    // case
    case: $ => seq('case', $._literal, ':', ),

    // default

    alt: $ => seq(
      field("pattern", $._pattern),
      '=>',
      field("expr", $._program),
      optional(';'),
    ),

    _record_expr: $ => choice(
      $.record,
      $.record_update,
      // $.record_punning,
    ),

    record: $ => common.block(seq(
      field("assignment", $.record_field),
      optional(seq(
        ',',
        common.sepEndBy(',', field("assignment", $._record_field)),
      )),
    )),

    record_update: $ => common.block(seq(
      // TODO: possible multiple spreads
      field("subject", $.spread),
      ',',
      common.sepEndBy1(',', field("field", $.record_field_path)),
    )),

    // ReasonLIGO disallows a record which consists of only one capture.
    // record_punning: $ => common.block(seq(
    //   field("assignment", $.capture),
    //   ',',
    //   common.sepEndBy1(',', field("assignment", $._record_field)),
    // )),

    _record_field: $ => choice(
      $.record_field,
      // $.capture,
    ),

    // capture: $ => field("accessor", $.FieldName),

    record_field: $ => seq(
      field("accessor", $.FieldName),
      ':',
      field("value", $._program),
    ),

    record_field_path: $ => seq(
      $._accessor_chain,
      ':',
      field("value", $._program),
    ),

    paren_expr: $ => prec(8, common.par(field("expr", $._annot_expr))),

    /// PREPROCESSOR

    // copied from reasonligo grammar.js
    preprocessor: $ => field("preprocessor_command", choice(
      $.p_include,
      $.p_if,
      $.p_error,
      $.p_define,
    )),

    p_include: $ => seq(
      '#',
      'include',
      field("filename", $.String)
    ),

    p_import: $ => seq(
      '#',
      'import',
      field("filename", $.String),
      field("alias", $.String),
    ),

    p_if: $ => choice(
      seq(
        '#',
        choice('if', 'elif', 'else'),
        field("rest", $._till_newline),
      ),
      seq(
        '#',
        'endif',
      ),
    ),

    p_error: $ => seq('#', 'error', field("message", $._till_newline)),
    p_define: $ => seq('#', choice('define', 'undef'), field("definition", $._till_newline)),

    /// MISCELLANEOUS UTILITIES

    block: $ => prec(1, common.block(
      seq(
        common.sepEndBy(';', choice(field("statement", $._statement), $.return)),
      )
    )),

    return: $ => seq($.Return, optional($._expr_term)),

    /// REGULAR EXPRESSIONS

    ConstrName: $ => $._NameCapital,
    ConstrNameDecl: $ => seq('"', $._NameCapital, '"'),
    FieldName: $ => $._Name,
    ModuleName: $ => $._NameCapital,
    TypeName: $ => $._Name,
    Name: $ => $._Name,
    NameDecl: $ => $._Name,
    TypeVariableName: $ => $._Name,

    _till_newline: $ => /[^\n]*\n/,

    attr: $ => choice(
      /\/\*\s+@[a-zA-Z][a-zA-Z0-9_:]*\s*\*\//,
      /\/\/\s+@[a-zA-Z][a-zA-Z0-9_:]*/),

    String: $ => /\"(\\.|[^"])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    PositiveInt: $ => /([1-9][0-9_]*|0)/,
    Tez: $ => seq($.PositiveInt, "as", "tez"),
    Nat: $ => seq($.PositiveInt, "as", "nat"),
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,
    Bool: $ => choice($.False, $.True),

    False: $ => 'false',
    True: $ => 'true',
    Unit: $ => 'unit',
    None: $ => 'None',
    Some: $ => 'Some',
    Export: $ => 'export',
    Return: $ => 'return',
    // skip: $ => 'skip',
    // rec: $ => 'rec',
    wildcard: $ => '_',
  }
});

// Fix patterns
// Add statements (if-else, switch, for, while, assignment...)
// 
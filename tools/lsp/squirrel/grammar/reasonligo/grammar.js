const PREC = {
  OR: 0,
  AND: 1,
  COMPARE: 3,
  CONCAT: 5,
  PLUS: 6,
  MINUS: 6,
  MUL: 7,
  DIV: 7,
  TYPE: 101,
  LET: 100,
};

const OPS = [
  ['+', PREC.PLUS],
  ['-', PREC.MINUS],
  ['mod', PREC.DIV],
  ['/', PREC.DIV],
  ['*', PREC.MUL],
  ['++', PREC.CONCAT],
  ['<', PREC.COMPARE],
  ['>', PREC.COMPARE],
  ['<=', PREC.COMPARE],
  ['>=', PREC.COMPARE],
  ['==', PREC.COMPARE],
  ['!=', PREC.COMPARE],
  ['&&', PREC.COMPARE],
  ['||', PREC.OR],
]

const common = require('../common.js')

module.exports = grammar({
  name: 'ReasonLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment],
  extras: $ => [$.ocaml_comment, $.comment, /\s/],

  conflicts: $ =>
    [[$._expr_term, $._pattern]
      , [$.Name, $.TypeName]
      , [$.annot_pattern, $.let_decl]
      , [$.lambda, $.tuple_pattern]
      , [$._expr_term, $.nullary_constr_pattern]
      , [$._expr_term, $.FieldName]
      , [$._expr_term, $.TypeName]
      , [$.FieldName, $.TypeName]
      , [$.FieldName, $.NameDecl]
      , [$.string_type, $._literal]
      , [$.Name, $.NameDecl]
      , [$.NameDecl, $.TypeName]
      , [$.Name, $.NameDecl, $.TypeName]
      , [$.list_pattern, $.Nil, $.list]
      , [$.list, $.Nil]
      , [$.list_pattern, $.Nil]
      , [$.TypeWildcard, $.wildcard]
      , [$._core_type, $._literal]
    ],

  rules: {
    source_file: $ => common.sepEndBy(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $.type_decl,
        $.let_decl,
        $.preprocessor,
       ),

    /// TYPE DECLARATIONS

    type_decl: $ =>
      seq(
        'type',
        field("type_name", $.TypeName),
        '=',
        field("type_value", $._type_expr),
      ),

    _type_expr: $ =>
      choice(
        $.TypeWildcard,
        $.fun_type,
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
          field("constructor", $.ConstrName),
          optional(common.par(field("arguments", $._type_expr))),
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
      prec.right(8,
        seq(
          field("domain", $._type_expr),
          '=>',
          field("codomain", $._type_expr),
        ),
      ),

    _core_type: $ =>
      choice(
        $.Int,
        $.TypeName,
        $.app_type,
        $.module_TypeName,
      ),

    app_type: $ =>
      seq(
        field("functor", $._core_type),
        field("arguments", $._type_arguments),
      ),

    _type_arguments: $ => choice(
      // $.michelson_tuple,
      common.par(common.sepBy(',', field("argument", $._type_expr))),
    ),

    michelson_tuple: $ => seq(
      '(',
      field("arg1", $._type_expr),
      ',',
      field("label1", $.String),
      ',',
      field("arg2", $._type_expr),
      ',',
      field("label2", $.String),
      ')',
    ),

    module_TypeName: $ =>
      seq(
        common.sepBy1('.', field("path", $.ModuleName)),
        '.',
        field("type", $.TypeName),
      ),

    tuple_type: $ =>
      common.par(common.sepBy1(',', field("element", $._type_expr))),

    string_type: $ => $.String,

    /// LET DECLARATIONS

    let_decl: $ => prec.left(PREC.LET, common.withAttrs($, seq(
      'let',
      optional(field("rec", $.rec)),
      common.sepBy1(',', field("binding", $._pattern)),
      optional(seq(
        ':',
        field("type", $._type_expr)
      )),
      '=',
      field("value", $._program),
    ))),

    /// STATEMENTS

    _statement: $ => prec(1, choice(
      $.let_decl,
      $.type_decl,
      $._expr,
    )),

    /// PATTERNS

    _pattern: $ =>
      choice(
        $.wildcard,
        $._literal,
        $.tuple_pattern,
        $.var_pattern,
        $.annot_pattern,
        $.unary_constr_pattern,
        $.nullary_constr_pattern,
        $.list_pattern,
        $.record_pattern,
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
        $.Nil,
        $.None,
      ),

    tuple_pattern: $ => prec(13, common.par(
      common.sepBy1(',', field("pattern", $._pattern)),
    )),

    var_pattern: $ => field("var", $.NameDecl),

    annot_pattern: $ => seq(
      field("subject", $._pattern),
      ':',
      field("type", $._type_expr),
    ),

    unary_constr_pattern: $ => prec(1, seq(
      field("constructor", $.ConstrName),
      field("arg", $._pattern),
    )),

    nullary_constr_pattern: $ => seq(
      field("constructor", $.ConstrName),
    ),

    list_pattern: $ => common.brackets(
      common.sepBy(',', field("pattern", $._spread_pattern)),
    ),

    _spread_pattern: $ => choice(
      $.spread_pattern,
      $._pattern,
    ),

    spread_pattern: $ => seq(
      '...',
      field("expr", $._pattern),
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
      field("body", $._pattern),
    ))),

    record_capture_pattern: $ => common.withAttrs($, prec(9, field("name", $.NameDecl))),

    /// PROGRAM

    _program: $ => prec(1, choice(
      $.let_in,
      $._expr
    )),

    let_in: $ => seq(
      field("declaration", $._declaration),
      ';',
      field("body", $._program),
    ),

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

    lambda: $ => prec.right(12, seq(
      common.par(common.sepBy(',', field("argument", $._pattern))),
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
        field("index", $._expr), // indices really aren't arbitrary expressions…
      )
    )),

    binary_call: $ => choice(
      ...OPS
        .map(([op, precendence]) =>
          prec.right(precendence, seq(
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
      $.block,
      $.tuple,
      $.list,
      $.data_projection,
      $.if,
      $.switch,
      $._record_expr,
      $.michelson_interop,
      $.paren_expr,
      $.let_in,
    ),

    tuple: $ => common.par(seq(
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
      ':',
      field("type", $._type_expr),
    ),

    list: $ => common.brackets(
      common.sepBy(',', field("element", $._spread_expr)),
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
      field("selector", $._expr),
      field("then", $.block),
      optional(seq(
        'else',
        field('else', $.block),
      ))
    ),

    switch: $ => seq(
      'switch',
      field("subject", $._expr_term),
      common.block(seq(
        optional('|'),
        common.sepBy('|', field("alt", $.alt)),
      ))
    ),

    alt: $ => seq(
      field("pattern", $._pattern),
      '=>',
      field("expr", $._program),
      optional(';'),
    ),

    _record_expr: $ => choice(
      $.record,
      $.record_update,
      $.record_punning,
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
    record_punning: $ => common.block(seq(
      field("assignment", $.capture),
      ',',
      common.sepEndBy1(',', field("assignment", $._record_field)),
    )),

    _record_field: $ => choice(
      $.record_field,
      $.capture,
    ),

    capture: $ => field("accessor", $.FieldName),

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

    michelson_interop: $ => seq(
      '[%Michelson',
      common.par(
        seq(
          field("code", $.michelson_code),
          ':',
          field("type", $._type_expr),
        )
      ),
      optional(common.par(common.sepBy(',', field("argument", $._expr)))),
      ']'
    ),

    michelson_code: $ => seq(
      '{|',
      repeat(/([^\|]|\|[^}])/),
      '|}'
    ),

    paren_expr: $ => prec(8, common.par(field("expr", $._annot_expr))),

    /// PREPROCESSOR

    preprocessor: $ => field("preprocessor_command", choice(
      $.include,
      $.p_if,
      $.p_error,
      $.p_warning,
      $.p_define,
    )),

    include: $ => seq(
      '#include',
      field("filename", $.String)
    ),

    p_if: $ => choice(
      seq(
        choice('#if', '#ifdef', '#ifndef', '#elif', '#else'),
        field("rest", $._till_newline),
      ),
      '#endif',
    ),

    p_error: $ => seq('#error', field("message", $._till_newline)),
    p_warning: $ => seq('#warning', field("message", $._till_newline)),
    p_define: $ => seq(choice('#define', '#undef'), field("definition", $._till_newline)),

    /// MISCELLANEOUS UTILITIES

    block: $ => prec(1, common.block(
      seq(
        common.sepEndBy(';', field("statement", $._statement)),
      )
    )),

    /// REGULAR EXPRESSIONS

    ConstrName: $ => $._NameCapital,
    FieldName: $ => $._Name,
    ModuleName: $ => $._NameCapital,
    TypeName: $ => $._Name,
    Name: $ => $._Name,
    NameDecl: $ => $._Name,

    _till_newline: $ => /[^\n]*\n/,

    attr: $ => /\[@[a-zA-Z][a-zA-Z0-9_:]*\]/,

    String: $ => /\"(\\.|[^"])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,
    Bool: $ => choice($.False, $.True),

    False: $ => 'false',
    True: $ => 'true',
    Unit: $ => seq('(', ')'),
    Nil: $ => seq('[', ']'),
    None: $ => 'None',
    Some: $ => 'Some',
    skip: $ => 'skip',
    rec: $ => 'rec',
    wildcard: $ => '_',
  }
});

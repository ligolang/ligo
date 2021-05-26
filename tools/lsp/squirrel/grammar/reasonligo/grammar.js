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

let sepBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)))
let sepBy = (sep, rule) => optional(sepBy1(sep, rule))

let sepEndBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)), optional(sep))
let sepEndBy = (sep, rule) => optional(sepEndBy1(sep, rule))

let par = x => seq('(', x, ')')
let opar = x => seq(optional('('), x, optional(')'))
let brackets = x => seq('[', x, ']')
let block = x => seq('{', x, '}')

let tuple = x => seq(x, ',', x, repeat(seq(',', x)))

let withAttrs = ($, x) => seq(field("attributes", repeat($.attr)), x)

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
      , [$.type_string, $._literal]
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
    source_file: $ => sepEndBy1(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      field("declaration",
        choice(
          $.type_decl,
          $.let_decl,
          $.preprocessor,
        )
      ),

    _program: $ => prec(1, choice(
      $.let_in,
      $._expr
    )),

    //// EXPRESSIONS ///////////////////////////////////////////////////////////

    let_decl: $ => prec.left(PREC.LET, withAttrs($, seq(
      'let',
      optional(field("rec", $.rec)),
      sepBy1(',', field("binding", $._pattern)),
      optional(seq(
        ':',
        field("type", $._type_expr)
      )),
      '=',
      field("value", $._program),
    ))),

    fun_type: $ =>
      prec.right(8,
        seq(
          field("domain", $._type_expr),
          '=>',
          field("codomain", $._type_expr),
        ),
      ),

    module_access: $ => seq(
      sepBy1('.', field("path", $.NameModule)),
      '.',
      field("field", $.FieldName),
    ),

    let_in: $ => seq(
      $._declaration,
      ';',
      field("body", $._program),
    ),

    _expr: $ => choice(
      $.lambda,
      $.indexing,
      $.binary_call,
      $.unary_call,
      $._expr_term,
      $.apply,
      $.Some_call,
      $.module_access,
    ),

    Some_call: $ => prec.right(10, seq(
      field("some", $.Some),
      field("argument", $._expr),
    )),

    apply: $ => prec.left(20, seq(
      field("function", $._expr),
      par(sepBy(',', field("argument", $._program))),
    )),

    _expr_term: $ => choice(
      $.block,
      $.tuple,
      $.list,
      $._literal,
      $.Name,
      $.ConstrName,
      $.data_projection,
      $.if,
      $.switch,
      $._record_expr,
      $.michelson_interop,
      $.paren_expr,
      $.let_in,
    ),

    paren_expr: $ => prec(8, par(field("expr", $._annot_expr))),

    michelson_code: $ => seq(
      '{|',
      repeat(/([^\|]|\|[^}])/),
      '|}'
    ),

    michelson_interop: $ => seq(
      '[%Michelson',
      par(
        seq(
          field("code", $.michelson_code),
          ':',
          field("type", $._type_expr),
        )
      ),
      optional(par(sepBy(',', field("argument", $._expr)))),
      ']'
    ),

    _record_expr: $ => choice(
      $.record,
      $.record_update,
      $.record_punning,
    ),

    record: $ => block(seq(
      field("assignment", $.record_field),
      optional(seq(
        ',',
        sepEndBy(',', field("assignment", $._record_field)),
      )),
    )),

    record_update: $ => block(seq(
      // TODO: possible multiple spreads
      field("subject", $.spread),
      ',',
      sepEndBy1(',', field("field", $.record_field_path)),
    )),

    // ReasonLIGO disallows a record which consists of only one capture.
    record_punning: $ => block(seq(
      field("assignment", $.capture),
      ',',
      sepEndBy1(',', field("assignment", $._record_field)),
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

    // The precedence is chosen so as to overtake over
    // 'field_decl'. The contract where it is relevant is
    // 'tools/lsp/squirrel/test/contracts/sexps/single_record_item.religo'.
    _accessor_chain: $ => prec.right(11, sepBy1('.', field("accessor", $.FieldName))),

    list: $ => brackets(
      sepBy(',', field("element", $._spread_expr)),
    ),

    _spread_expr: $ => choice(
      $._expr,
      $.spread,
    ),

    spread: $ => seq(
      '...',
      field("name", $._expr),
    ),

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
      block(seq(
        optional('|'),
        sepBy('|', field("alt", $.alt)),
      ))
    ),

    alt: $ => seq(
      field("pattern", $._pattern),
      '=>',
      field("expr", $._program),
      optional(';'),
    ),

    // a.attribute
    data_projection: $ => seq(
      field("expr", $.Name),
      '.',
      $._accessor_chain,
    ),

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

    // Workaround to make operators to be statements as well
    // so that they won't be stripped when naming them as fields
    ...OPS.reduce(
      (acc, [e, _]) => ({ ...acc, [e]: $ => seq(e) }), {}
    ),

    unary_call: $ => prec.right(8, seq(field("negate", $.negate), field("arg", $._expr_term))),

    negate: $ => choice('-', '!'),

    indexing: $ => prec.right(12, seq(
      field("box", $._expr),
      brackets(
        field("index", $._expr), // indices really aren't arbitrary expressions…
      )
    )),

    block: $ => prec(1, block(
      seq(
        sepEndBy(';', field("statement", $._statement)),
      )
    )),

    _statement: $ => prec(1, choice(
      $.let_decl,
      $.type_decl,
      $._expr,
    )),

    tuple: $ => par(seq(
      field("item", $._annot_expr),
      ',',
      sepBy1(',', field("item", $._annot_expr)),
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

    lambda: $ => prec.right(12, seq(
      par(sepBy(',', field("argument", $._pattern))),
      optional(seq(
        ':',
        field("type", $._type_expr),
      )),
      '=>',
      field("body", $._program),
    )),

    //// TYPES /////////////////////////////////////////////////////////////////

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
        $._core_type,
        $.type_tuple,
        $.sum_type,
        $.record_type,
        $.type_string,
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

    _core_type: $ =>
      choice(
        $.type_application,
        $.TypeName,
        $.Int,
        $.module_TypeName,
      ),

    module_TypeName: $ =>
      seq(
        sepBy1('.', field("path", $.NameModule)),
        '.',
        field("type", $.TypeName),
      ),

    type_application: $ =>
      seq(
        field("functor", $._core_type),
        field("arguments", $._type_arguments),
      ),

    _type_arguments: $ => choice(
      // $.michelson_tuple,
      par(sepBy(',', field("argument", $._type_expr))),
    ),

    type_string: $ => $.String,

    type_tuple: $ =>
      par(sepBy1(',', field("element", $._type_expr))),

    sum_type: $ => choice(
      sepBy1('|', field("variant", $.variant)),
      withAttrs($, seq('|', sepBy1('|', field("variant", $.variant)))),
    ),

    variant: $ =>
      prec.left(8,
        withAttrs($, seq(
          field("constructor", $.ConstrName),
          optional(par(field("arguments", $._type_expr))),
        )),
      ),

    record_type: $ => withAttrs($,
      block(sepEndBy1(',', field("field", $.field_decl)))
    ),

    field_decl: $ =>
      prec(10, // see 'accessor_chain' for explanation of precedence
        withAttrs($, seq(
          field("field_name", $.FieldName),
          ':',
          field("field_type", $._type_expr),
        ))),

    //// PATTERNS //////////////////////////////////////////////////////////////

    _pattern: $ =>
      choice(
        $.tuple_pattern,
        $._literal,
        $.var_pattern,
        $.annot_pattern,
        $.wildcard,
        $.unary_constr_pattern,
        $.nullary_constr_pattern,
        $.list_pattern,
        $.record_pattern,
      ),

    var_pattern: $ => field("var", $.NameDecl),

    nullary_constr_pattern: $ => seq(
      field("constructor", $.ConstrName),
    ),

    unary_constr_pattern: $ => prec(1, seq(
      field("constructor", $.ConstrName),
      field("arg", $._pattern),
    )),

    constr_pattern: $ => seq(
      field("ctor", $.ConstrName),
      optional(seq(
        '(',
        sepBy(',', field("arg", $._pattern)),
        ')',
      )),
    ),

    record_pattern: $ => withAttrs($, block(
      sepEndBy1(",", field("field", $._record_field_pattern)),
    )),

    _record_field_pattern: $ => choice(
      $.record_field_pattern,
      $.record_capture_pattern,
    ),

    record_field_pattern: $ => withAttrs($, prec(9, seq(
      field("name", $.FieldName),
      ":",
      field("body", $._pattern),
    ))),

    record_capture_pattern: $ => withAttrs($, prec(9, field("name", $.NameDecl))),

    annot_pattern: $ => seq(
      field("subject", $._pattern),
      ':',
      field("type", $._type_expr),
    ),

    tuple_pattern: $ => prec(13, par(
      sepBy1(',', field("pattern", $._pattern)),
    )),

    list_pattern: $ => brackets(
      sepBy(',', field("pattern", $._spread_pattern)),
    ),

    _spread_pattern: $ => choice(
      $.spread_pattern,
      $._pattern,
    ),

    spread_pattern: $ => seq(
      '...',
      field("expr", $._pattern),
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


    /// Preprocessor

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

    /// Literals

    FieldName: $ => $.Name,
    ConstrName: $ => $._NameCapital,
    fun_name: $ => $.Name,
    struct_name: $ => $.Name,
    var: $ => $.Name,
    module_name: $ => $._NameCapital,
    _ident: $ => $.Name,

    _till_newline: $ => /[^\n]*\n/,

    attr: $ => /\[@[a-zA-Z][a-zA-Z0-9_:]*\]/,

    String: $ => /\"(\\.|[^"])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,
    Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    NameDecl: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    NameModule: $ => $._NameCapital,
    TypeName: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    TypeWildcard: $ => '_',
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
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
    rec: $ => 'rec',
  }
});

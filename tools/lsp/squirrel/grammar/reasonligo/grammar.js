const PREC = {
  OR: 0,
  AND: 1,
  COMPARE: 3,
  CONCAT: 5,
  PLUS: 6,
  MINUS: 6,
  MUL: 7,
  DIV: 7,
  ATTR: 1,
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

let sepBy1 = (sep, rule) =>
  seq(
    rule,
    repeat(seq(sep, rule)),
    optional(sep)
  )

let sepBy = (sep, rule) => optional(sepBy1(sep, rule))

let par = x => seq('(', x, ')')
let opar = x => seq(optional('('), x, optional(')'))
let brackets = x => seq('[', x, ']')
let block = x => seq('{', x, '}')

let tuple = x => seq(x, ',', x, repeat(seq(',', x)))
let list__ = x => brackets(sepBy(';', x))

let nseq = x => seq(x, repeat(x))

module.exports = grammar({
  name: 'ReasonLigo',

  word: $ => $.Keyword,
  extras: $ => [$.oneline_comment, $.block_comment, /\s/, $.attr],

  conflicts: $ =>
    [[$._expr_term, $._pattern]
      , [$._expr_term, $.var_pattern]
      , [$.Name, $.TypeName]
      , [$._expr_term, $.module_name]
      , [$.annot_pattern, $.let_declaration]
      , [$.lambda, $.tuple_pattern]
      , [$._expr_term, $.nullary_constr_pattern]
      , [$.list, $.list_pattern]
      , [$._expr_term, $.lhs]
      , [$.FieldName, $.lhs]
      , [$._expr_term, $.capture]
      , [$.type_string, $._literal]
      , [$.Name, $.NameDecl]
      , [$.NameDecl, $.TypeName]
      , [$.Name, $.NameDecl, $.TypeName]
    ],

  rules: {
    contract: $ => sepBy1(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      field("declaration",
        choice(
          $.type_decl,
          $.let_declaration,
          $.include,
        )
      ),

    //// EXPRESSIONS ///////////////////////////////////////////////////////////

    let_declaration: $ => prec.left(PREC.LET, seq(
      'let',
      optional(field("rec", $.rec)),
      sepBy1(',', field("binding", $._pattern)),
      optional(seq(
        ':',
        field("type", $._type_expr)
      )),
      '=',
      field("value", $._expr),
    )),

    fun_type: $ =>
      prec.right(8,
        seq(
          field("domain", $._type_expr),
          '=>',
          field("codomain", $._type_expr),
        ),
      ),

    module_TypeName: $ =>
      seq(
        $.module_name,
        '.',
        $.TypeName,
      ),

    _expr: $ => choice(
      $.lambda,
      $.indexing,
      $.binary_call,
      $.unary_call,
      $._expr_term,
      $.apply,
      $.Some_call,
    ),

    Some_call: $ => prec.right(10, seq(
      field("some", $.Some),
      field("argument", $._expr),
    )),

    apply: $ => prec.left(20, seq(
      field("function", $._expr),
      par(sepBy(',', field("argument", $._expr))),
    )),

    _expr_term: $ => choice(
      $.block,
      $.tuple,
      $.list,
      $._literal,
      $.Name,
      $.Name_Capital,
      $.data_projection,
      $.if,
      $.switch,
      $.record,
    ),

    record: $ => block(
      seq(
        // TODO: possible multiple spreads
        optional(seq(field("assignment", $.spread), ',')),
        sepBy(',', field("assignment", $._record_field)),
      ),
    ),

    _record_field: $ => choice(
      $.record_field,
      $.capture,
    ),

    capture: $ => field("name", $.Name),

    record_field: $ => seq(
      field("name", $.lhs),
      ':',
      field("value", $._expr),
    ),

    lhs: $ => seq(
      field("callee", $.Name),
      optional(seq(field("name", $.Name), '.')),
    ),

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
      field("expr", $._expr),
      optional(';'),
    ),

    // a.attribute
    data_projection: $ => prec.right(21, seq(
      field("box", $._expr),
      ".",
      sepBy1('.', field("selector", $.FieldName)),
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
        sepBy(';', field("statement", $._statement)),
        optional(';'),
      )
    )),

    _statement: $ => prec(1, choice(
      $.let_declaration,
      $._expr,
    )),

    tuple: $ =>
      par(sepBy1(',', field("item", $._annot_expr))),

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
      field("body", $._expr),
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
        $.type_application,
        $.TypeName,
        // $.module_TypeName,
      ),

    module_TypeName: $ =>
      seq(
        field("module", $.module_name),
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

    sum_type: $ =>
      prec.left(8,
        seq(
          optional('|'),
          sepBy1('|', field("variant", $.variant)),
        ),
      ),

    variant: $ =>
      prec.left(8,
        seq(
          field("constructor", $.Name_Capital),
          optional(par(field("arguments", $._type_expr))),
        ),
      ),

    record_type: $ =>
      block(sepBy(',', field("field", $.field_decl))),

    field_decl: $ =>
      seq(
        field("field_name", $.FieldName),
        ':',
        field("field_type", $._type_expr),
      ),

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
      ),

    var_pattern: $ => field("var", $.NameDecl),

    nullary_constr_pattern: $ => seq(
      field("constructor", $.Name_Capital),
    ),

    unary_constr_pattern: $ => prec(1, seq(
      field("constructor", $.Name_Capital),
      field("arg", $._pattern),
    )),

    constr_pattern: $ => seq(
      field("ctor", $.Name_Capital),
      optional(seq(
        '(',
        sepBy(',', field("arg", $._pattern)),
        ')',
      )),
    ),

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


    ///////////////////////////////////////////

    FieldName: $ => $.Name,
    fun_name: $ => $.Name,
    struct_name: $ => $.Name,
    var: $ => $.Name,
    module_name: $ => $.Name_Capital,
    _ident: $ => $.Name,

    comment: $ => choice(
      $.oneline_comment,
      $.block_comment
    ),

    oneline_comment: $ => token(seq('//', /.*/)),

    block_comment: $ => seq(
      '/*',
      repeat(/./),
      '*/'
    ),

    include: $ => seq('#include', $.String),

    String: $ => /\"(\\.|[^"])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,
    Name: $ => /[a-z][a-zA-Z0-9_]*/,
    NameDecl: $ => /[a-z][a-zA-Z0-9_]*/,
    TypeName: $ => /[a-z][a-zA-Z0-9_]*/,
    Name_Capital: $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword: $ => /[A-Za-z][a-z]*/,
    Bool: $ => choice($.False, $.True),

    include: $ => seq('#include', field("filename", $.String)),

    False: $ => 'false',
    True: $ => 'true',
    Unit: $ => '()',
    Nil: $ => '[]',
    None: $ => 'None',
    Some: $ => 'Some',
    skip: $ => 'skip',
    rec: $ => 'rec',
    wildcard: $ => '_',
    rec: $ => 'rec',

    attr: $ => seq('[@', $.Name, ']'),
  }
});

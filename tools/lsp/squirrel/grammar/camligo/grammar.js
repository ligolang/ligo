const common = require('../common.js')

function mkOp($, opExpr) {
  return seq(
    field("left", $._expr),
    field("op", opExpr),
    field("right", $._expr)
  );
}

// These rules were extracted as they require a function argument
// Thanks, I hate it
const sum_type_rules = suffix => ({
  // Cat of string | Person of string * string
  ['sum_type' + suffix]: $ => prec.left(10,
    choice(
      common.sepBy1('|', field("variant", $['variant' + suffix])),
      common.withAttrs($, seq('|', common.sepBy1('|', field("variant", $['variant' + suffix])))),
    )
  ),

  // Person of string * string
  ['variant' + suffix]: $ => common.withAttrs($, seq(
    field("constructor", $.ConstrName),
    optional(seq(
      "of",
      field("type", $[suffix])
    ))
  )),
})

module.exports = grammar({
  name: 'CameLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, /\s/],

  rules: {
    source_file: $ => repeat(field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $._decl,
        $.preprocessor,
      ),

    _decl: $ =>
      choice(
        $.type_decl,
        $.let_decl,
        $.fun_decl,
        $.module_decl,
        $.module_alias,
      ),

    /// TYPE DECLARATION

    type_decl: $ => seq(
      "type",
      optional(field("params", $._type_params)),
      field("name", $.TypeName),
      "=",
      field("type", $._type_expr)
    ),

    _type_params: $ => choice(
      $.type_param,
      $.type_params,
    ),

    type_param: $ => field("param", $.var_type),

    type_params: $ => common.par(
      common.sepBy1(",", field("param", $.var_type)),
    ),

    // { field1 : a; field2 : b }
    record_type: $ => common.withAttrs($, seq(
      "{",
      common.sepBy(";", field("field", $.field_decl)),
      optional(";"),
      "}"
    )),

    // field : string * int
    field_decl: $ => common.withAttrs($, seq(
      field("field", $._label),
      ":",
      field("type", $._type_expr)
    )),

    ...sum_type_rules('_prod_type_level'),

    _lambda_app_type: $ => choice(
      $._prod_type_level,
      $.sum_type_prod_type_level,
    ),

    _prod_type_level: $ => choice(
      $.prod_type,
      $._type_core,
    ),

    _fun_type_level: $ => choice(
      $.fun_type,
      $._prod_type_level,
    ),

    _type_core: $ => choice(
      $.Int,
      $.TypeWildcard,
      $.string_type,
      $.TypeName,
      $.tuple_type,
      $.app_type,
      $.record_type,
      $.module_TypeName,
      $.var_type,
    ),

    ...sum_type_rules('_fun_type_level'),

    _type_expr: $ => choice(
      $._fun_type_level,
      $.sum_type_fun_type_level,
    ),

    var_type: $ => seq(
      "'",
      field("name", $.TypeVariableName),
    ),

    // int -> string
    fun_type: $ => prec.right(8, seq(
      field("domain", $._prod_type_level),
      "->",
      field("codomain", $._fun_type_level)
    )),

    // string * integer
    prod_type: $ => prec.right(5, seq(
      field("x", $._type_core),
      common.some(seq(
        "*",
        field("x", $._type_core)
      ))
    )),

    // a t, (a, b) t
    app_type: $ => prec(10, seq(
      field("x", choice(
        $._type_core,
      )),
      field("f", $.TypeName)
    )),

    tuple_type: $ => common.par(seq(
      common.sepBy1(",", field("x", $._type_expr)),
    )),

    string_type: $ => field("value", $.String),

    module_TypeName: $ =>
      seq(
        common.sepBy1('.', field("path", $.ModuleName)),
        '.',
        field("type", $.TypeName),
      ),

    /// FUNCTION DECLARATION

    fun_decl: $ => common.withAttrs($, seq(
      "let",
      optional(field("recursive", "rec")),
      field("name", $.NameDecl),
      // TODO: we treat arguments as an annotated pattern. Once we set up proper
      // argument and its type resolution, this must be changed.
      common.some(field("arg", $._irrefutable)),
      optional(seq(
        ":",
        field("type", $._type_expr)
      )),
      "=",
      field("body", $._program),
    )),

    /// LET DECLARATION

    let_decl: $ => common.withAttrs($, seq(
      "let",
      optional(field("recursive", "rec")),
      field("name", $._irrefutable),
      optional(seq(
        ":",
        field("type", $._type_expr)
      )),
      "=",
      field("body", $._program),
    )),

    /// MODULES

    module_decl: $ => seq(
      "module",
      field("moduleName", $.ModuleName),
      "=",
      "struct",
      repeat(field("declaration", $._declaration)),
      "end"
    ),

    module_alias: $ => seq(
      "module",
      field("moduleName", $.ModuleName),
      "=",
      common.sepBy1('.', field("module", $.ModuleName))
    ),

    /// PROGRAM

    _program: $ => choice(
      $.let_in,
      $._expr
    ),

    let_in: $ => seq(
      field("decl", $._decl),
      "in",
      field("body", $._program)
    ),

    /// PATTERNS

    _pattern: $ => choice(
      $._literal,
      $.Verbatim,
      $.constr_pattern,
      $.list_pattern,
      $.list_con_pattern,
      $._paren_pattern,
      $.tuple_pattern,
      $.record_pattern,
      $.var_pattern,
      $.wildcard_pattern,
    ),

    constr_pattern: $ => prec(10,
      seq(
        field("ctor", $.ConstrName),
        optional(field("args", $._pattern))
      )
    ),

    // [1;2]
    list_pattern: $ => seq(
      "[",
      common.sepEndBy(';', field("item", $._pattern)),
      "]"
    ),

    // a :: b
    list_con_pattern: $ => prec.right(9, seq(
      field("x", $._pattern),
      "::",
      field("xs", $._pattern)
    )),

    _paren_pattern: $ => choice(
      $.par_annot_pattern,
      $.paren_pattern,
    ),

    par_annot_pattern: $ => choice(
      seq(
        "(",
        field("pat", $._pattern),
        ":",
        field("type", $._type_expr),
        ")",
      ),
    ),

    paren_pattern: $ =>
      seq(
        "(",
        field("pat", $._pattern),
        ")"
      ),

    // a, b, c
    tuple_pattern: $ => prec.right(8, seq(
      field("item", $._pattern),
      ",",
      common.sepBy1(",", field("item", $._pattern))
    )),

    // { field1 = pat_a ; field2 = pat_b; field3 }
    record_pattern: $ => common.withAttrs($, seq(
      "{",
      common.sepBy(";", field("field", $._record_field_pattern)),
      optional(";"),
      "}"
    )),

    _record_field_pattern: $ => choice(
      $.record_field_pattern,
      $.record_capture_pattern,
    ),

    // field = _pattern
    record_field_pattern: $ => common.withAttrs($, prec(9, seq(
      field("name", $._label),
      "=",
      field("body", $._pattern),
    ))),

    // field
    record_capture_pattern: $ => common.withAttrs($, prec(9, field("name", $.NameDecl))),

    var_pattern: $ => seq(
      field("var", $.NameDecl)
    ),

    wildcard_pattern: $ => "_",

    /// IRREFUTABLES

    _irrefutable: $ => choice(
      $._sub_irrefutable,
      $.irrefutable_tuple,
    ),

    _sub_irrefutable: $ => choice(
      $.ConstrName,
      $.Unit,
      $._closed_irrefutable,
      $.record_pattern,
      $.var_pattern,
      $.wildcard_pattern,
    ),

    // a, b, c
    irrefutable_tuple: $ => prec.right(8, seq(
      field("item", $._sub_irrefutable),
      ",",
      common.sepBy1(",", field("item", $._sub_irrefutable)),
    )),

    _closed_irrefutable: $ => choice(
      $.annot_pattern,
      $.closed_irrefutable,
    ),

    annot_pattern: $ =>
      seq(
        "(",
        field("pat", $._irrefutable),
        ":",
        field("type", $._type_expr),
        ")"
      ),

    closed_irrefutable: $ => common.par(field("pat", choice(
      $._irrefutable,
      $.constr_pattern,
    ))),

    /// EXPRESSIONS

    _expr: $ => choice(
      $._call,
      $.tup_expr,
      $._sub_expr,
    ),

    _call: $ => choice(
      $.unary_op_app,
      $.binary_op_app,
    ),

    // - a
    unary_op_app: $ => prec(19, seq(
      field("negate", choice("-", "not")),
      field("arg", $._expr)
    )),

    binary_op_app: $ => choice(
      prec.right(17, mkOp($, choice("lsl", "lsr"))),
      prec.left(16, mkOp($, choice("/", "*", "mod", "land", "lor", "lxor"))),
      prec.left(15, mkOp($, choice("-", "+"))),
      prec.right(14, mkOp($, "::")),
      prec.right(13, mkOp($, "^")),
      prec.left(12, mkOp($, choice("=", "<>", "<", "<=", ">", ">="))),
      prec.left(11, mkOp($, "&&")),
      prec.left(10, mkOp($, choice("or", "||"))),
    ),

    tup_expr: $ => prec.right(9, seq(
      field("x", $._expr),
      common.some(seq(
        ",",
        field("x", $._expr),
      )),
    )),

    _sub_expr: $ => choice(
      $._core_expr,
      $.fun_app,
      $.if_expr,
      $.lambda_expr,
      $.match_expr,
    ),

    _core_expr: $ => choice(
      $.ConstrName,
      $.Name,
      $._literal,
      $.Verbatim,
      $.paren_expr,
      $.annot_expr,
      $.record_expr,
      $.record_literal,
      $.list_expr,
      $.data_projection,
      $.module_access,
      $.block_expr,
      $.code_inj,
    ),

    // f a
    fun_app: $ => prec.left(20, seq(
      field("f", $._core_expr),
      repeat1(field("x", $._core_expr))
    )),

    paren_expr: $ => seq(
      "(",
      field("expr", $._program),
      ")"
    ),

    annot_expr: $ => seq(
      "(",
      field("expr", $._program),
      ":",
      field("type", $._type_expr),
      ")",
    ),

    // { p with a.x = b; c = d }
    record_expr: $ => seq(
      "{",
      field("subject", $._path),
      "with",
      common.sepEndBy1(";", field("field", $.record_path_assignment)),
      "}"
    ),

    // a.x = b;
    // a = b;
    record_path_assignment: $ => seq(
      $._path,
      "=",
      field("value", $._expr),
    ),

    _path: $ => choice($.Name, $.data_projection),

    // { a = b; c = d }
    record_literal: $ => seq(
      "{",
      common.sepEndBy1(";", field("field", $.record_assignment)),
      "}"
    ),

    // a = b;
    record_assignment: $ => seq(
      field("accessor", $.FieldName),
      "=",
      field("value", $._expr),
    ),

    // if a then b else c
    if_expr: $ => prec.right(seq(
      "if",
      field("condition", $._expr),
      "then",
      field("then", $._program),
      optional(seq(
        "else",
        field("else", $._program)
      ))
    )),

    lambda_expr: $ => common.withAttrs($, seq(
      "fun",
      repeat1(field("arg", $._sub_irrefutable)),
      optional(seq(
        ":",
        field("type", $._lambda_app_type),
      )),
      "->",
      field("body", $._program)
    )),

    // match x with ...
    match_expr: $ => prec.right(1, seq(
      "match",
      field("subject", $._expr),
      "with",
      optional('|'),
      common.sepBy('|', field("alt", $.matching))
    )),

    // Dog as x -> f x
    matching: $ => seq(
      field("pattern", $._pattern),
      "->",
      field("body", $._program)
    ),

    list_expr: $ => seq(
      "[",
      common.sepEndBy(";", field("item", $._expr)),
      "]"
    ),

    // a.0 or a.attribute
    data_projection: $ => prec.right(21, seq(
      field("box", $.Name),
      ".",
      $._accessor_chain,
    )),

    // field names (or indices) separated by a dot
    _accessor_chain: $ => prec.right(common.sepBy1('.', field("accessor", $._accessor))),

    _accessor: $ => choice($.FieldName, $.Int),

    module_access: $ => seq(
      common.sepBy1('.', field("path", $.ModuleName)),
      '.',
      field("field", $.FieldName),
    ),

    block_expr: $ => seq(
      "begin",
      common.sepBy(";", field("item", $._program)),
      "end",
    ),

    code_inj: $ => seq(
      "[%",
      field("lang", $.Attr),
      field("code", $._expr),
      ']',
    ),

    /// PREPROCESSOR

    // I (@heitor.toledo) decided to keep the preprocessors here since we still
    // attempt to parse the contract even if `ligo preprocess` failed.
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

    _label: $ => $.FieldName,

    _literal: $ => choice(
      $.String,
      $.Int,
      $.Nat,
      $.Tez,
      $.Bytes,
      $.True,
      $.False,
      $.Unit
    ),

    /// REGULAR EXPRESSIONS

    ConstrName: $ => $._NameCapital,
    FieldName: $ => $._Name,
    ModuleName: $ => $._NameCapital,
    TypeName: $ => $._Name,
    Name: $ => $._Name,
    NameDecl: $ => $._Name,
    TypeVariableName: $ => $._Name,

    _till_newline: $ => /[^\n]*\n/,

    Attr: $ => /[a-zA-Z][a-zA-Z0-9_:.@%]*/,

    String: $ => /\"(\\.|[^"\n])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Verbatim: $ => seq('{|', repeat(/([^\|]|\|[^}])/), '|}'),
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,

    False: $ => 'false',
    True: $ => 'true',
    Unit: $ => seq('(', ')'),
  }
});

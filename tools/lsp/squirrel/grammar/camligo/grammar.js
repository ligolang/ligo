let sepBy1 = (sep, p) => seq(p, repeat(seq(sep, p)))
let sepBy = (sep, p) => optional(sepBy1(sep, p))

let sepEndBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)), optional(sep))
let sepEndBy = (sep, rule) => optional(sepEndBy1(sep, rule))

let some = x => seq(x, repeat(x))

let par = x => seq('(', x, ')')

let withAttrs = ($, x) => seq(field("attributes", repeat($.attr)), x)

function mkOp($, opExpr) {
  return seq(
    field("left", $._expr),
    field("op", opExpr),
    field("right", $._expr)
  );
}

module.exports = grammar({
  name: 'CameLigo',
  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment],
  extras: $ => [$.ocaml_comment, $.comment, /\s/],

  rules: {
    source_file: $ => repeat(field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $.type_decl,
        $.let_decl,
        $.fun_decl,
        $.preprocessor,
      ),

    fun_decl: $ => withAttrs($, seq(
      "let",
      optional(field("recursive", "rec")),
      field("name", $.NameDecl),
      // TODO: we treat arguments as an annotated pattern. Once we set up proper
      // argument and its type resolution, this must be changed.
      some(field("arg", $._irrefutable)),
      optional(seq(
        ":",
        field("type", $._type_expr)
      )),
      "=",
      field("body", $._program),
    )),

    let_decl: $ => withAttrs($, seq(
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

    //========== EXPR ============

    _program: $ => choice(
      $.let_in,
      $.type_decl,
      $._expr
    ),

    let_in: $ => seq(
      field("decl", choice($.let_decl, $.fun_decl)),
      "in",
      field("body", $._program)
    ),

    // [1;2]
    list_pattern: $ => seq(
      "[",
      sepBy(';', field("item", $._pattern)),
      "]"
    ),

    // a :: b
    list_con_pattern: $ => prec.right(9, seq(
      field("x", $._pattern),
      "::",
      field("xs", $._pattern)
    )),

    // a, b, c
    tup_pattern: $ => prec.right(8, seq(
      field("item", $._pattern),
      ",",
      sepBy1(",", field("item", $._pattern))
    )),

    _pattern: $ => choice(
      $.var_pattern,
      $._paren_pattern,
      $.con_pattern,
      $._literal,
      $.list_pattern,
      $.list_con_pattern,
      $.tup_pattern,
      $.rec_pattern,
      $.wildcard_pattern,
    ),

    // { field1 = pat_a ; field2 = pat_b; field3 }
    rec_pattern: $ => withAttrs($, seq(
      "{",
      sepBy(";", field("field", $._rec_field_pattern)),
      optional(";"),
      "}"
    )),

    _rec_field_pattern: $ => choice(
      $.rec_field_pattern,
      $.rec_capture_pattern,
    ),

    // field = _pattern
    rec_field_pattern: $ => withAttrs($, prec(9, seq(
      field("name", $._label),
      "=",
      field("body", $._pattern),
    ))),

    // field
    rec_capture_pattern: $ => withAttrs($, prec(9, field("name", $.NameDecl))),

    _irrefutable: $ => choice(
      $._sub_irrefutable,
      $.irrefutable_tuple,
    ),

    // a, b, c
    irrefutable_tuple: $ => prec.right(8, seq(
      field("item", $._sub_irrefutable),
      ",",
      sepBy1(",", field("item", $._sub_irrefutable)),
    )),

    _sub_irrefutable: $ => choice(
      $.var_pattern,
      $.wildcard_pattern,
      $.Unit,
      $.rec_pattern,
      $.ConstrName,
      $._closed_irrefutable,
    ),

    _closed_irrefutable: $ => choice(
      $.annot_pattern,
      $.closed_irrefutable,
    ),

    closed_irrefutable: $ => par(field("pat", choice(
      $._irrefutable,
      $.con_pattern,
    ))),

    wildcard_pattern: $ => "_",

    var_pattern: $ => seq(
      field("var", $.NameDecl)
    ),

    con_pattern: $ => prec(10,
      seq(
        field("ctor", $.ConstrName),
        optional(field("args", $._pattern))
      )
    ),

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

    annot_pattern: $ =>
      seq(
        "(",
        field("pat", $._irrefutable),
        ":",
        field("type", $._type_expr),
        ")"
      ),

    _call: $ => choice(
      $.unary_op_app,
      $.binary_op_app,
    ),

    binary_op_app: $ => choice(
      prec.left(16, mkOp($, "mod")),
      prec.left(15, mkOp($, choice("/", "*"))),
      prec.left(14, mkOp($, choice("-", "+"))),
      prec.right(13, mkOp($, "::")),
      prec.right(12, mkOp($, "^")),
      prec.left(11, mkOp($, choice("&&", "||"))),
      prec.left(10, mkOp($, choice("=", "<>", "==", "<", "<=", ">", ">="))),
    ),

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
      optional(par(sepBy(',', field("argument", $._sub_expr)))),
      ']'
    ),

    // - a
    unary_op_app: $ => prec(19, seq(
      field("negate", "-"),
      field("arg", $._expr)
    )),

    // f a
    fun_app: $ => prec.left(20, seq(
      field("f", $._sub_expr),
      field("x", $._sub_expr)
    )),

    _path: $ => choice($.Name, $.data_projection),

    module_access: $ => seq(
      sepBy1('.', field("path", $.NameModule)),
      '.',
      field("field", $.FieldName),
    ),

    _accessor: $ => choice($.FieldName, $.Int),

    // field names (or indices) separated by a dot
    _accessor_chain: $ => prec.right(sepBy1('.', field("accessor", $._accessor))),

    // a.0 or a.attribute
    data_projection: $ => prec.right(21, seq(
      field("box", $.Name),
      ".",
      $._accessor_chain,
    )),

    // { a = b; c = d }
    rec_literal: $ => seq(
      "{",
      sepEndBy1(";", field("field", $.rec_assignment)),
      "}"
    ),

    // { p with a.x = b; c = d }
    rec_expr: $ => seq(
      "{",
      field("subject", $._path),
      "with",
      sepEndBy1(";", field("field", $.rec_path_assignment)),
      "}"
    ),

    // a.x = b;
    // a = b;
    rec_path_assignment: $ => seq(
      $._path,
      "=",
      field("value", $._expr),
    ),

    // a = b;
    rec_assignment: $ => seq(
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

    // match x with ...
    match_expr: $ => prec.right(1, seq(
      "match",
      field("subject", $._expr),
      "with",
      optional('|'),
      sepBy('|', field("alt", $.matching))
    )),

    // Dog as x -> f x
    matching: $ => seq(
      field("pattern", $._pattern),
      "->",
      field("body", $._program)
    ),

    lambda_expr: $ => seq(
      "fun",
      repeat1(field("arg", $._irrefutable)),
      "->",
      field("body", $._program)
    ),

    list_expr: $ => seq(
      "[",
      sepBy(";", field("item", $._expr)),
      "]"
    ),

    tup_expr: $ => prec.right(9, seq(
      field("x", $._expr),
      some(seq(
        ",",
        field("x", $._expr),
      )),
    )),

    _expr: $ => choice(
      $._call,
      $._sub_expr,
      $.tup_expr,
    ),

    _sub_expr: $ => choice(
      $.fun_app,
      $.paren_expr,
      $.annot_expr,
      $.Name,
      $.ConstrName,
      $._literal,
      $.rec_expr,
      $.rec_literal,
      $.if_expr,
      $.lambda_expr,
      $.match_expr,
      $.list_expr,
      $.data_projection,
      $.module_access,
      $.block_expr,
      $.michelson_interop,
    ),

    block_expr: $ => seq(
      "begin",
      sepBy(";", field("item", $._program)),
      "end",
    ),

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

    // a t, (a, b) t
    type_app: $ => prec(10, seq(
      field("x", $._type_expr),
      field("f", $.TypeName)
    )),

    type_tuple: $ => seq(
      "(",
      sepBy1(",", field("x", choice($._type_expr, $.String))),
      ")"
    ),

    // string * integer
    type_product: $ => prec.right(5, seq(
      field("x", $._type_expr),
      some(seq(
        "*",
        field("x", $._type_expr)
      ))
    )),

    // int -> string
    type_fun: $ => prec.right(8, seq(
      field("domain", $._type_expr),
      "->",
      field("codomain", $._type_expr)
    )),

    module_TypeName: $ =>
      seq(
        sepBy1('.', field("path", $.NameModule)),
        '.',
        field("type", $.TypeName),
      ),

    _type_expr: $ => choice(
      $.TypeWildcard,
      $.type_fun,
      $.type_product,
      $.type_app,
      $.TypeName,
      $.type_tuple,
      $.Int,
      $.module_TypeName,
    ),

    // Cat of string, Person of string * string
    variant: $ => withAttrs($, seq(
      field("constructor", $.ConstrName),
      optional(seq(
        "of",
        field("type", $._type_expr)
      ))
    )),

    type_sum: $ =>
      prec.left(10,
        seq(choice(
          sepBy1('|', field("variant", $.variant)),
          withAttrs($, seq('|', sepBy1('|', field("variant", $.variant)))),
        ))
      ),

    _label: $ => $.FieldName,

    // field : string * int
    type_rec_field: $ => withAttrs($, seq(
      field("field", $._label),
      ":",
      field("type", $._type_expr)
    )),

    // { field1 : a; field2 : b }
    type_rec: $ => withAttrs($, seq(
      "{",
      sepBy(";", field("field", $.type_rec_field)),
      optional(";"),
      "}"
    )),

    _type_def_body: $ => choice(
      $.type_sum,
      $._type_expr,
      $.type_rec
    ),

    type_decl: $ => seq(
      "type",
      field("name", $.TypeName),
      "=",
      field("type", $._type_def_body)
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

    attr: $ => /\[@[a-zA-Z][a-zA-Z0-9_:]*\]/,

    String: $ => /\"(\\.|[^"])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,
    Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    TypeName: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    TypeWildcard: $ => '_',
    NameDecl: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    NameModule: $ => /[A-Z][a-zA-Z0-9_]*/,
    NameWildcard: $ => '_',
    FieldName: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    ConstrName: $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword: $ => /[A-Za-z][a-z]*/,

    False: $ => 'false',
    True: $ => 'true',
    Unit: $ => seq('(', ')'),

    _till_newline: $ => /[^\n]*\n/,
  }
});

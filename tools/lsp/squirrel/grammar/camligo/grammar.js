let sepBy1 = (sep, p) => seq(p, repeat(seq(sep, p)))
let sepBy  = (sep, p) => optional(sepBy1(sep, p))

function mkOp($, opExpr) {
  return seq(
    field("arg1", $._expr),
    field("op", opExpr),
    field("arg2", $._expr)
  );
}

module.exports = grammar({
  name: 'CameLigo',
  word:   $ => $.Keyword,
  extras: $ => [$.ocaml_comment, $.comment, /\s/],

  rules: {
    contract: $ => repeat($._declaration),

    _declaration: $ => choice(
      $.let_decl,
      $.type_decl,
      $.include,
    ),

    include: $ => seq(
      '#include',
      field("filename", $.String)
    ),

    _attribute: $ => /\[@@[a-z]+\]/,

    let_decl: $ => seq(
      "let",
      field("name", $._binder),
      optional(seq(
        ":",
        field("type", $.type_expr)
      )),
      "=",
      field("body",$._program),
      repeat(field("attribute", $._attribute))
    ),

    _binder: $ => choice(
      $.func_header,
      $._pattern
    ),

    //========== EXPR ============

    _program: $ => choice(
      $.let_expr1,
      $._expr
    ),

    func_header: $ => prec(1, seq(
      optional(field("recursive", "rec")),
      field("name", $.Name),
      repeat(field("arg", $.paren_pattern))
    )),

    let_expr1: $ => seq(
      $.let_decl,
      "in",
      field("innerExpr", $._program)
    ),

    // [1;2]
    list_pattern: $ => seq(
      "[",
      sepBy(';', field("patternListItem", $._pattern)),
      "]"
    ),

    // a :: b
    list_con_pattern: $ => prec.right(9, seq(
      field("patX", $._pattern),
      "::",
      field("patXs", $._pattern)
    )),

    // a, b, c
    tup_pattern: $ => prec.right(8,seq(
      field("tuplePatternItem", $._pattern),
      ",",
      sepBy1(",", field("tuplePatternItem", $._pattern))
    )),

    _pattern: $ => choice(
      $.Name,
      $.paren_pattern,
      $.con_pattern,
      $._literal,
      $.list_pattern,
      $.list_con_pattern,
      $.tup_pattern,
      "_"
    ),

    con_pattern: $ => prec(10,
      seq(
        field("conPattern", $.data_con),
        optional(field("conArgPattern",$._pattern))
      )
    ),

    paren_pattern: $ => seq(
      "(",
      field("innerPattern", $._pattern),
      optional(seq(
        ":",
        $.type_expr,
      )),
      ")"
    ),

    call: $ => choice(
      $.unary_op_app,
      $._mod_op_app,
      $._mul_op_app,
      $._add_op_app,
      $._list_con_op_app,
      $._string_cat_op_app,
      $._bool_op_app,
      $._comp_op_app
    ),

    _mod_op_app: $ => prec.left(16, mkOp($, "mod")),
    _mul_op_app: $ => prec.left(15, mkOp($, choice("/", "*"))),
    _add_op_app: $ => prec.left(14, mkOp($, choice("-", "+"))),
    _list_con_op_app: $ => prec.right(13, mkOp($, "::")),
    _string_cat_op_app: $ => prec.right(12, mkOp($, "^")),
    _bool_op_app: $ => prec.left(11, mkOp($, choice("&&", "||"))),
    _comp_op_app: $ => prec.left(10, mkOp($, choice("=", "<>", "==", "<", "<=", ">", ">="))),

    // - a
    unary_op_app: $ => prec(19, choice(
       seq(field("unaryOp", "-"), field("arg", $._expr))),
    ),

    // f a
    fun_app: $ => prec.left(20, seq(field("appF", $._sub_expr), field("appArg",$._sub_expr))),

    // a.0
    index_accessor: $ => prec.right(21, seq(field("exp", $._sub_expr), ".", field("ix", $._sub_expr))),

    // { p with a = b; c = d }
    rec_expr: $ => seq(
      "{",
      optional(seq(field("updateTarget", $.Name), "with")),
      field("assignment", $.rec_assignment),
      repeat(seq(";", field("assignment", $.rec_assignment))),
      optional(";"),
      "}"
    ),
    // a = b;
    rec_assignment: $ => seq(
      field("assignmentLabel", $._expr),
      "=",
      field("assignmentExpr", $._expr),
    ),

    // if a then b else c
    if_expr: $ => prec.right(seq(
      "if",
      field("condition", $._expr),
      "then",
      field("thenBranch", $._program),
      optional(seq(
        "else",
        field("elseBranch", $._program)
      ))
    )),

    // match x with ...
    match_expr: $ => prec.right(1,seq(
      "match",
      field("matchTarget", $._expr),
      "with",
      optional('|'),
      sepBy('|', field("matching", $.matching))
    )),

    // Dog as x -> f x
    matching: $ => seq(
      field("pattern", $._pattern),
      "->",
      field("matchingExpr", $._program)
    ),

    lambda_expr: $ => seq(
      "fun",
      repeat1(field("arg", $.paren_pattern)),
      "->",
      field("body", $._expr)
    ),

    list_expr: $ => seq(
      "[",
      sepBy(";", field("item", $._expr)),
      "]"
    ),

    tup_expr: $ => prec.right(9,seq(
      field("fst", $._expr),
      ",",
      field("snd", $._expr),
    )),

    _expr: $ => choice(
      $.call,
      $._sub_expr,
      $.tup_expr
    ),

    _sub_expr: $ => choice(
      $.fun_app,
      $.paren_expr,
      $.Name,
      $.Name_Capital,
      $._literal,
      $.rec_expr,
      $.if_expr,
      $.lambda_expr,
      $.match_expr,
      $.list_expr,
      $.index_accessor,
      $.block_expr,
    ),

    block_expr: $ => seq(
      "begin",
      sepBy(";", field("elem", $._program)),
      "end",
    ),

    paren_expr: $ => seq(
      "(",
      field("innerExpr", $._program),
      optional(seq(
        ":",
        field("annotExpr", $.type_expr)
      )),
      ")"
    ),


    //========== TYPE_EXPR ============
    // t, test, string, integer
    type_con: $ => $.TypeName,
    // Red, Green, Blue, Cat
    data_con: $ => $.Name_Capital,
    // a t, (a, b) t
    type_app: $ => prec(10,seq(
      choice(
        field("argument", $.type_expr),
        seq(
          "(",
          sepBy1(",", field("argument", choice($.type_expr, $.String))),
          ")"
        )
      ),
      field("typeAppCon", $.type_con)
    )),
    // string * integer
    type_product: $ => prec.right(5, seq(
      field("fst", $.type_expr),
      "*",
      field("snd", $.type_expr)
    )),

    // int -> string
    type_fun: $ => prec.right(8, seq(
      field("domain", $.type_expr),
      "->",
      field("codomain", $.type_expr)
    )),

    type_expr: $ => choice(
      $.type_fun,
      $.type_product,
      $.type_app,
      $.type_con,
      $.paren_type_expr,
    ),

    paren_type_expr: $ => seq(
      "(",
      field("innerTypeExpr", $.type_expr),
      ")"
    ),

    // Cat of string, Person of string * string
    variant: $ => seq(
      field("constructor", $.data_con),
      optional(seq(
        "of",
        field("constructor_data", $.type_expr)
      ))
    ),

    // Cat of string | Personn of string * string
    type_sum: $ => seq(
      optional('|'),
      sepBy1('|', field("variant", $.variant)),
    ),

    // field : string * int
    _label: $ => $.FieldName,

    type_rec_field: $ => seq(
      field("recLabel", $._label),
      ":",
      field("labelType", $.type_expr)
    ),

    // { field1 : a; field2 : b }
    type_rec: $ => seq(
      "{",
      sepBy(";", field("recField", $.type_rec_field)),
      optional(";"),
      "}"
    ),

    type_def_body: $ => choice(
      $.type_sum,
      $.type_expr,
      $.type_rec
    ),

    type_def: $ => seq(
      field("typeName", $.type_con),
      "=",
      field("typeValue", $.type_def_body)
    ),

    type_decl: $ => seq(
      "type",
      field("typeDef", $.type_def)
    ),

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

    String:       $ => /\"(\\.|[^"])*\"/,
    Int:          $ => /-?([1-9][0-9_]*|0)/,
    Nat:          $ => /([1-9][0-9_]*|0)n/,
    Tez:          $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes:        $ => /0x[0-9a-fA-F]+/,
    Name:         $ => /[a-z][a-zA-Z0-9_]*/,
    TypeName:     $ => /[a-z][a-zA-Z0-9_]*/,
    FieldName:    $ => /[a-z][a-zA-Z0-9_]*/,
    Name_Capital: $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword:      $ => /[A-Za-z][a-z]*/,

    False:         $ => 'false',
    True:          $ => 'true',
    Unit:          $ => '()',

    comment: $ => /\/\/(\*\)[^\n]|\*[^\)\n]|[^\*\n])*\n/,

    ocaml_comment: $ =>
      seq(
        '(*',
        repeat(choice(
          $.ocaml_comment,
          /[^\*\(]/,
          /\*[^\)]/,
          /\([^\*]/,
        )),
        /\*+\)/
      ),
  }
});

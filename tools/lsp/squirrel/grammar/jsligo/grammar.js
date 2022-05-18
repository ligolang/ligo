const common = require('../common.js')

module.exports = grammar({
  name: 'JsLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker, $._js_ligo_attribute, $._automatic_semicolon],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, $._js_ligo_attribute, /\s/],

  conflicts: $ => [
    [$.variant],
    [$.module_access],
    [$._expr_statement, $.data_projection],
    [$.annot_expr, $.fun_arg],
    [$.variant, $.string_type],
    [$._member_expr, $.Nat, $.Tez],
    [$.Int, $.Nat, $.Tez],
    [$._binding_pattern, $._list_item_pattern],
    [$._member_expr, $._binding_pattern],
    [$.Name, $.NameDecl],
    [$.Name, $.FieldName],
    [$.tuple, $.list_pattern],
    [$.ConstrName, $.ModuleName]
  ],

  rules: {
    source_file: $ =>
      common.sepEndBy(optional($._semicolon), field("toplevel", $._statement_or_namespace_or_preprocessor)),

    _statement_or_namespace_or_preprocessor: $ => choice(field("statement", $._statement), $.namespace_statement, $.preprocessor),

    namespace_statement: $ => prec.left(2, seq(
      optional(field("export", 'export')), 'namespace', field("moduleName", $.ModuleName),
      '{',
      common.sepEndBy($._semicolon, field("declaration", $._statement_or_namespace_or_preprocessor)),
      '}'
    )),

    _statement: $ => field("statement", $._base_statement),

    _base_statement: $ => prec(5, choice(
      $._expr_statement,
      $.return_statement,
      $.block_statement,
      $.switch_statement,
      $.import_statement,
      $.export_statement,
      $._declaration_statement,
      $.if_else_statement,
      $.for_of_statement,
      $.while_statement,
      $.break_statement
    )),

    break_statement: $ => 'break',

    _expr_statement: $ => choice(
      $.lambda,
      $.assignment_operator,
      $.binary_call,
      $.unary_call,
      $.apply,
      $.list_literal,
      $._member_expr,
      $.pattern_match,
      $.type_as_annotation,
    ),

    list_literal: $ => seq('list', common.par($._list_elements)),

    assignment_operator: $ => prec.right(2,
      seq(field("lhs", $._expr_statement),
      field("op", choice('=', '*=', '/=', '%=', '+=', '-=')),
      field("rhs", $._expr_statement))
    ),

    type_as_annotation: $ => seq(field("subject", $._expr_statement), 'as', field("type", $._core_type)),

    binary_call: $ => choice(
      prec.left(4,  seq(field("left", $._expr_statement), field("op", '||'), field("right", $._expr_statement))),
      prec.left(5,  seq(field("left", $._expr_statement), field("op", '&&'), field("right", $._expr_statement))),
      prec.left(10, seq(field("left", $._expr_statement), field("op", choice('<', '<=', '>', '>=', '==', '!=')), field("right", $._expr_statement))),
      prec.left(12, seq(field("left", $._expr_statement), field("op", choice('+', '-')),      field("right", $._expr_statement))),
      prec.left(13, seq(field("left", $._expr_statement), field("op", choice('*', '/', '%')), field("right", $._expr_statement)))
    ),

    unary_call: $ => prec.right(15, seq(field("negate", choice('-', '!')), field("arg", $._expr_statement))),

    apply: $ => prec.right(2, seq(field("function", $._apply), $._arguments)),

    _arguments: $ => common.par(common.sepBy(',', field("argument", $._annot_expr))),

    _apply: $ => prec(5, choice($.apply, $._member_expr)),

    _expr: $ => choice($._expr_statement, $._record_expr),

    _annot_expr: $ => choice(
      $.annot_expr,
      $._expr,
    ),

    annot_expr: $ => seq(
      field("subject", $._expr),
      $._type_annotation
    ),

    pattern_match: $ => seq(
      'match',
      common.par(
        seq(
          field("subject", $._member_expr),
          ',',
          choice($._list_cases, $._ctor_cases)
        )
      )
    ),

    _list_cases: $ => seq('list',
      common.par(
        common.brackets(
          common.sepBy1(',',
            field("alt", $.list_case)
          )
        )
      )
    ),

    list_case: $ => seq(
      field("pattern", common.par(seq($._binding_pattern, $._type_annotation))),
      '=>',
      field("expr", $.body)
    ),

    _ctor_cases: $ => common.block(
      common.sepBy1(',',
        field("alt", $.ctor_case)
      )
    ),

    ctor_param: $ => seq(field("subject", $._binding_pattern), field("type", $._type_annotation)),

    ctor_case: $ => seq(
      field("pattern", $.constr_pattern),
      '=>',
      field("expr", $.body)
    ),

    constr_pattern: $ => seq(
      field("constructor", $.ConstrName),
      ':',
      field("arg", common.par(optional($.ctor_param)))
    ),

    _member_expr: $ => choice(
      $.Name,
      $.Int,
      $.Bytes,
      $.String,
      $.Nat,
      $.Tez,
      $._Bool,
      $.Unit_kwd,
      $.ConstrName,
      $.data_projection,
      $.indexing,
      $.michelson_interop,
      $.paren_expr,
      $.module_access,
      $.tuple,
    ),

    tuple: $ => common.brackets(common.sepBy(',', field("item", $._annot_expr))),

    paren_expr: $ => common.par(field("expr", $._expr)),

    data_projection: $ => seq(field("field", $._member_expr), '.', $._accessor_chain),

    indexing: $ => prec(5, seq(field("box", $._member_expr), common.brackets(field("index", $._expr)))),

    _accessor_chain: $ => prec.right(common.sepBy1('.', field("accessor", $.Name))),

    michelson_interop: $ => seq(
      '(Michelson',
        seq(
          field("code", $.michelson_code),
          'as',
          field("type", $._type_expr),
        ),
      ')'
    ),

    michelson_code: $ => seq('`', repeat(/([^\|]|\|[^}])/), '`'),

    module_access: $ => seq(
      common.sepBy1('.', field("path", $.ModuleName)),
      '.',
      field("field", $.FieldName),
    ),

    _list_elements: $ => common.brackets(common.sepBy(',', field("element", $._list_item))),

    _list_item: $ => choice($._annot_expr, $.spread),

    lambda: $ => choice(
      seq(
        common.par(common.sepBy1(',', field("argument", $.fun_arg))),
        optional(field("type", $._type_annotation)), '=>',
        field("body", $.body),
      ),
      seq(
        '(', ')',
        optional(field("type", $._type_annotation)), '=>',
        field("body", $.body),
      ),
      seq(
        field("argument", $.Name), '=>',
        field("body", $.body),
      )
    ),

    body: $ => prec.right(3,
      choice(
        $.block_statement,
        $._expr_statement
      )
    ),

    _statements: $ => common.sepEndBy1(optional($._semicolon), $._statement),

    _statements_strict_semicolon: $ => common.sepEndBy1($._semicolon, $._statement),

    _type_annotation: $ => seq(':', seq(optional($.type_params), field("type", $._type_expr))),

    fun_arg: $ => seq(field("argument", $._binding_pattern), field("type", $._type_annotation)),

    return_statement: $ => prec.right(seq('return', optional(field("expr", $._expr)))),

    block_statement: $ => prec.left(2, seq('{', $._statements, '}')),

    _record_expr: $ => choice($.record, $.record_update),

    record: $ => common.block(seq(
      field("assignment", $._record_field),
      optional(seq(
        ',',
        common.sepEndBy(',', field("assignment", $._record_field)),
      ))
    )),

    _record_field: $ => choice($.record_field, $.capture),

    record_field: $ => seq(
      field("accessor", $.property_name),
      ':',
      field("value", $._expr)
    ),

    capture: $ => field("accessor", $.Name),

    record_update: $ => common.block(seq(
      field("subject", $.spread),
      ',',
      common.sepEndBy1(',', field("field", $._record_field)),
    )),

    spread: $ => seq('...', field("name", $._expr_statement)),

    property_name: $ => choice($.Int, $.String, $.ConstrName, $.Name),

    _type_expr: $ => choice(
      $.fun_type,
      $.sum_type,
      $._core_type
    ),

    fun_type: $ => seq(field("domain", common.par(common.sepBy(',', $._fun_param))), '=>', field("codomain", $._type_expr)),

    _fun_param: $ => seq($._Name, field("type", $._type_annotation)),

    sum_type: $ => prec.right(1, common.withAttrs($, seq(optional('|'), common.sepBy1('|', field("variant", $.variant))))),

    variant: $ => common.withAttrs($, common.brackets(
      choice(
        field("constructor", $.String),
        seq(field("constructor", $.String), ',', field("arguments", common.sepBy1(',', field("ctor_argument", $._type_expr))))
      )
    )),

    _core_type: $ => choice(
      $.Int,
      $.TypeWildcard,
      $.TypeName,
      $.string_type,
      $.module_access_t,
      $.record_type,
      $.app_type,
      common.withAttrs($, $.tuple_type),
      common.par($._type_expr)
    ),

    string_type: $ => field("value", $.String),

    module_access_t: $ => seq(common.sepBy1('.', field("path", $.ModuleName)), '.', field("type", $.Name)),

    record_type: $ => common.withAttrs($, common.block(common.sepEndBy(',', field("field", $.field_decl)))),

    field_decl: $ => common.withAttrs($, choice(
      field("field_name", $.FieldName),
      seq(field("field_name", $.FieldName), field("field_type", $._type_annotation))
    )),

    app_type: $ => prec(3, seq(field("functor", $.TypeName), common.chev(common.sepBy1(',', field("argument", $._type_expr))))),

    tuple_type: $ => common.brackets(common.sepBy1(',', field("element", $._type_expr))),

    import_statement: $ => seq('import', field("moduleName", $.ModuleName), '=', common.sepBy1('.', field("module", $.ModuleName))),

    export_statement: $ => seq(field("export", 'export'), $._declaration_statement),

    _declaration_statement: $ => choice(
      $._let_decls,
      $._const_decls,
      $.type_decl
    ),

    type_decl: $ => seq("type", field("type_name", $.TypeName), optional(field("params", $.type_params)), '=', field("type_value", $._type_expr)),

    type_params: $ => common.chev(common.sepBy1(',', field("param", $.var_type))),

    var_type: $ => field("name", $.TypeVariableName),

    _let_decls: $ => common.withAttrs($, seq('let', common.sepBy1(',', $.let_decl))),

    _const_decls: $ => common.withAttrs($, seq('const', common.sepBy1(',', $.const_decl))),

    let_decl: $ => $._binding_initializer,

    const_decl: $ => $._binding_initializer,

    _binding_initializer: $ => seq(
      field("binding", $._binding_pattern),
      optional(
        seq(
          optional($.type_params),
          field("type", $._type_annotation)
        )
      ),
      '=',
      field("value", $._expr)
    ),

    _binding_pattern: $ => choice(
      $.var_pattern,
      $.wildcard,
      $.record_pattern,
      $.list_pattern,
      $.tuple_pattern,
      $.Unit_kwd
    ),

    var_pattern: $ => field("var", $.NameDecl),

    record_pattern: $ => common.withAttrs($, common.block(
      common.sepEndBy1(",", field("field", $._record_field_pattern)),
    )),

    _record_field_pattern: $ => choice(
      $.record_field_pattern,
      $.record_capture_pattern,
      $.record_rest_pattern,
    ),

    record_field_pattern: $ => common.withAttrs($, prec(9, seq(
      field("name", $.FieldName),
      ":",
      field("body", $._binding_pattern),
    ))),

    record_capture_pattern: $ => field("name", $.NameDecl),

    record_rest_pattern: $ => seq('...', field("name", $.NameDecl)),

    list_pattern: $ => choice(
      seq('[', ']'),
      common.brackets(
        seq(
          common.sepBy1(',', field("pattern", $._list_item_pattern)),
          ',',
          field("pattern", $.spread_pattern)
        )
      ),
    ),

    _list_item_pattern: $ => choice(
      $.var_pattern,
      $.wildcard,
      $.list_pattern
    ),

    spread_pattern: $ => seq('...', field("expr", $._list_item_pattern)),

    tuple_pattern: $ => common.brackets(common.sepBy1(',', field("pattern", $._binding_pattern))),

    switch_statement: $ => prec.left(seq(
      'switch',
      common.par(field("selector", $._expr)),
      common.block($._cases),
      optional($._semicolon)
    )),

    _cases: $ => choice(
      seq(repeat1(field("case", $.case_statement)), optional(field("case", $.default_statement))),
      field("case", $.default_statement)
    ),

    case_statement: $ => seq('case', field("selector_value", $._expr), $._case_statements),

    default_statement: $ => seq('default', $._case_statements),

    _case_statements: $ => seq(':', optional(field("body", $._statements_strict_semicolon))),

    if_else_statement: $ => prec.right(seq('if', field("selector", common.par($._expr)), field("then", $._base_statement), optional($._else_branch))),

    _else_branch: $ => seq('else', field("else", $._statement)),

    for_of_statement: $ => seq('for', common.par(seq($._index_kind, field("key", $.Name), 'of', field("collection", $._expr_statement))), field("body", $._statement)),

    _index_kind: $ => choice($.Let_kwd, $.Const_kwd),

    while_statement: $ => seq('while', field("breaker", common.par($._expr)), field("body", $._statement)),

    /// PREPROCESSOR

    // copied from cameligo/grammar.js

    preprocessor: $ => field("preprocessor_command", choice(
      $.p_include,
      $.p_import,
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

    _semicolon: $ => choice(';', $._automatic_semicolon),

    ConstrName: $ => $._NameCapital,
    FieldName: $ => $._Name,
    ModuleName: $ => $._NameCapital,
    TypeName: $ => choice($._Name, $._NameCapital),
    TypeVariableName: $ => choice($._Name, $._NameCapital),
    Name: $ => $._Name,
    NameDecl: $ => $._Name,

    _till_newline: $ => /[^\n]*\n/,

    attr: $ => $._js_ligo_attribute,

    String: $ => /\"(\\.|[^"])*\"/,
    _Int: $ => /-?([1-9][0-9_]*|0)/,
    Int: $ => $._Int,
    Nat: $ => seq($._Int, 'as', 'nat'),
    Tez: $ => seq($._Int, 'as', choice('tez', 'mutez')),
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,
    _Bool: $ => choice($.False_kwd, $.True_kwd),

    Unit_kwd: $ => 'unit',
    False_kwd: $ => 'false',
    True_kwd: $ => 'true',
    wildcard: $ => '_',
    Let_kwd: $ => 'let',
    Const_kwd: $ => 'const',
  }
})
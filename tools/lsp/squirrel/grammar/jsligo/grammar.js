const common = require('../common.js')

module.exports = grammar({
  name: 'JsLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker, $._js_ligo_attribute, $._automatic_semicolon],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, $._js_ligo_attribute, /\s/],

  conflicts: $ => [
    [$.variant],
    [$.module_access],
    [$._expr_statement, $.projection],
    [$.annot_expr, $.parameter]
  ],

  rules: {
    source_file: $ =>
      common.sepEndBy(optional($._semicolon), field("toplevel", $._statement_or_namespace_or_preprocessor)),

    _statement_or_namespace_or_preprocessor: $ => choice($._statement, $.namespace_statement, $.preprocessor),

    namespace_statement: $ => prec.left(2, seq(
      optional(field("export", 'export')), 'namespace', field("moduleName", $.ModuleName),
      '{',
      common.sepEndBy($._semicolon, field("declaration", $._statement_or_namespace_or_preprocessor)),
      '}',
      optional($._automatic_semicolon)
    )),

    _statement: $ => prec.right(1, field("statement", choice($._base_statement, $.if_statement))),

    if_statement: $ => seq('if', field("selector", common.par($._expr)), field("then", $._statement)),

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
      $.fun_expr,
      $.type_as_annotation,
      $.assignment_operator,
      $.binary_operator,
      $.unary_operator,
      $.call_expr,
      $._member_expr,
      $.match_expr
    ),

    assignment_operator: $ => prec.right(2,
      seq(field("lhs", $._expr_statement),
      field("op", choice('=', '*=', '/=', '%=', '+=', '-=')),
      field("rhs", $._expr_statement))
    ),

    type_as_annotation: $ => seq(field("subject", $._expr_statement), 'as', field("type", $._core_type)),

    binary_operator: $ => choice(
      prec.left(4,  seq(field("left", $._expr_statement), field("op", '||'), field("right", $._expr_statement))),
      prec.left(5,  seq(field("left", $._expr_statement), field("op", '&&'), field("right", $._expr_statement))),
      prec.left(10, seq(field("left", $._expr_statement), field("op", choice('<', '<=', '>', '>=', '==', '!=')), field("right", $._expr_statement))),
      prec.left(12, seq(field("left", $._expr_statement), field("op", choice('+', '-')),      field("right", $._expr_statement))),
      prec.left(13, seq(field("left", $._expr_statement), field("op", choice('*', '/', '%')), field("right", $._expr_statement)))
    ),

    unary_operator: $ => prec.right(15, seq(field("negate", choice('-', '!')), field("arg", $._expr_statement))),

    call_expr: $ => prec.right(2, seq(field("f", $.lambda), $.arguments)),

    arguments: $ => common.par(common.sepBy(',', field("argument", $._annot_expr))),

    lambda: $ => prec(5, choice($.call_expr, $._member_expr)),

    _expr: $ => choice($._expr_statement, $.object_literal),

    _annot_expr: $ => choice(
      $.annot_expr,
      $._expr,
    ),

    annot_expr: $ => seq(
      field("subject", $._expr),
      $.type_annotation
    ),

    match_expr: $ => seq('match', common.par(seq(field("subject", $._member_expr), ',', field("alt", choice($._list_cases, $._ctor_cases))))),

    _list_cases: $ => seq('list',
      common.par(
        common.brackets(
          common.sepBy1(',',
            $.list_case
          )
        )
      )
    ),

    list_case: $ => seq(
      field("pattern", common.par(seq($.array_literal, optional($.type_annotation)))),
      '=>',
      field("body", $.body)
    ),

    _ctor_cases: $ => common.block(
      common.sepBy1(',',
        $.ctor_case
      )
    ),

    ctor_param: $ => seq($._expr, $.type_annotation),

    ctor_case: $ => seq(
      field("pattern", $.ConstrName),
      ':',
      common.par(common.sepBy(',', $.ctor_param)),
      '=>',
      field("body", $.body)
    ),

    _member_expr: $ => choice(
      $.Name,
      $.Int,
      $.Bytes,
      $.String,
      $._Bool,
      $.ctor_expr,
      $.projection,
      $.michelson_interop,
      $.paren_expr,
      $.module_access,
      $.array_literal,
      $.wildcard
    ),

    paren_expr: $ => common.par(field("expr", $._annot_expr)),

    ctor_expr: $ => seq(field("ctor", $.ConstrName), field("args", $.ctor_args)),

    ctor_args: $ => common.par(common.sepBy(',', $._expr)),

    projection: $ => choice(
      seq($._member_expr, common.brackets($._expr)),
      seq($._member_expr, '.', $.Name)
    ),

    michelson_interop: $ => seq(
      '(Michelson',
        seq(
          field("code", $.michelson_code),
          'as',
          field("type", $._core_type),
        ),
      ')'
    ),

    michelson_code: $ => seq('`', repeat(/([^\|]|\|[^}])/), '`'),

    module_access: $ => seq(
      common.sepBy1('.', field("path", $.ModuleName)),
      '.',
      field("field", $.FieldName),
    ),

    array_literal: $ => choice(common.brackets(common.sepBy(',', field("element", $._array_item)))),

    _array_item: $ => choice($._annot_expr, $.array_item_rest_expr),

    array_item_rest_expr: $ => seq('...', field("expr", $._expr)),

    fun_expr: $ => choice(
      seq(
        common.par($._parameters),
        optional(field("type", $.type_annotation)), '=>',
        field("body", $.body),
      ),
      seq(
        '(', ')',
        optional(field("type", $.type_annotation)), '=>',
        field("body", $.body),
      ),
      seq(
        field("argument", $.Name), '=>',
        field("body", $.body),
      )
    ),

    body: $ => prec.right(3, choice(seq('{', $._statements, '}', optional($._automatic_semicolon)), $._expr_statement)),

    _statements: $ => common.sepEndBy1($._semicolon, $._statement),

    type_annotation: $ => seq(':', field("type", $._type_expr)),

    _parameters: $ => common.sepBy1(',', field("argument", $.parameter)),

    parameter: $ => seq(field("expr", $._expr), field("type", $.type_annotation)),

    return_statement: $ => prec.left(2, seq('return', field("expr", optional($._expr)))),

    block_statement: $ => prec.left(2, seq('{', $._statements, '}', optional($._automatic_semicolon))),

    object_literal: $ => common.block(common.sepBy(',', $.property)),

    property: $ => choice($.Name, seq(field("property_name", $.property_name), ':', field("expr", $._expr)), $._property_spread),

    _property_spread: $ => seq('...', field("spread_expr", $._expr_statement)),

    property_name: $ => choice($.Int, $.String, $.ConstrName, $.Name),

    _type_expr: $ => choice(
      $.fun_type,
      $.sum_type,
      $._core_type
    ),

    fun_type: $ => seq(field("param_types", common.par(common.sepBy(',', $.fun_param))), '=>', field("return_type", $._type_expr)),

    fun_param: $ => seq(field("param_name", $.Name), $.type_annotation),

    sum_type: $ => prec.right(1, common.withAttrs($, seq(optional('|'), common.sepBy1('|', field("variant", $.variant))))),

    variant: $ => common.withAttrs($, common.brackets(
      choice(
        seq('"', field("constructor", $.ConstrName), '"'),
        seq('"', field("constructor", $.ConstrName), '"', ',', field("arguments", common.sepBy1(',', $._type_expr)))
      )
    )),

    _core_type: $ => choice(
      $.Int,
      $.wildcard,
      $.TypeName,
      $.module_access_t,
      $.object_type,
      $.type_ctor_app,
      common.withAttrs($, $.type_tuple),
      common.par($._type_expr)
    ),

    module_access_t: $ => seq(common.sepBy1('.', field("path", $.ModuleName)), '.', field("type", $.Name)),

    object_type: $ => common.withAttrs($, common.block(common.sepEndBy(',', $.field_decl))),

    field_decl: $ => common.withAttrs($, choice(
      field("field", $.FieldName),
      seq(field("field", $.FieldName), field("type", $.type_annotation))
    )),

    type_ctor_app: $ => prec(3, seq(field("type", $.TypeName), common.chev(common.sepBy1(',', field("argument", $._type_expr))))),

    type_tuple: $ => common.brackets(common.sepBy1(',', field("type_element", $._type_expr))),

    import_statement: $ => seq('import', field("alias_name", $.ModuleName), '=', common.sepBy1('.', field("module_path", $.ModuleName))),

    export_statement: $ => seq(field("export", 'export'), $._declaration_statement),

    _declaration_statement: $ => choice(
      $.let_decl,
      $.const_decl,
      $.type_decl
    ),

    type_decl: $ => seq("type", $.TypeName, optional($.type_params), '=', $._type_expr),

    type_params: $ => common.chev(common.sepBy1(',', field("type_param", $.TypeVariableName))),

    let_decl: $ => common.withAttrs($, seq('let', field("binding_list", $._binding_list))),

    const_decl: $ => common.withAttrs($, seq('const', field("binding_list", $._binding_list))),

    _binding_list: $ => common.sepBy1(',', field("binding", $._binding_initializer)),

    _binding_initializer: $ => seq($._binding_pattern, optional($.type_annotation), '=', $._expr),

    _binding_pattern: $ => choice(
      $.var_pattern,
      $.wildcard,
      $.object_pattern,
      $.array_pattern
    ),

    var_pattern: $ => common.withAttrs($, $.Name),

    object_pattern: $ => common.block($.property_patterns),

    property_patterns: $ => choice(
      $.property_pattern,
      seq($.property_patterns, ',', $.property_pattern),
      seq($.property_patterns, ',', $.object_rest_pattern)
    ),

    property_pattern: $ => choice(
      seq($.Name, '=', $._expr),
      seq($.Name, ':', $._binding_initializer),
      $.var_pattern,
    ),

    object_rest_pattern: $ => seq('...', $.Name),

    array_pattern: $ => common.brackets($._array_item_patterns),

    _array_item_patterns: $ => choice(
      $._array_item_pattern,
      seq($._array_item_patterns, ',', $._array_item_pattern),
      seq($._array_item_patterns, ',', $.array_rest_pattern)
    ),

    _array_item_pattern: $ => choice(
      $.var_pattern,
      $.wildcard,
      $.array_pattern
    ),

    array_rest_pattern: $ => seq('...', field("name", $.Name)),

    switch_statement: $ => seq('switch', common.par(field("selector", $._expr)), field("cases", common.block($._cases))),

    _cases: $ => choice(
      seq(repeat1($.case), optional($.default_case)),
      $.default_case
    ),

    case: $ => seq('case', field("selector_value", $._expr), field("body", $._case_statements)),

    default_case: $ => seq('default', field("body", $._case_statements)),

    _case_statements: $ => seq(':', choice(
      optional($._statements),
      $.block_statement,
    )),

    if_else_statement: $ => seq('if', field("selector", common.par($._expr)), field("then", $._base_statement), 'else', field("else", $._statement)),

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
    ConstrNameDecl: $ => seq('"', $._NameCapital, '"'),
    FieldName: $ => $._Name,
    ModuleName: $ => $._NameCapital,
    TypeName: $ => $._Name,
    Name: $ => $._Name,
    NameDecl: $ => $._Name,
    TypeVariableName: $ => $._Name,

    _till_newline: $ => /[^\n]*\n/,

    attr: $ => $._js_ligo_attribute,

    String: $ => /\"(\\.|[^"])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    _PositiveInt: $ => /([1-9][0-9_]*|0)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,
    _Bool: $ => choice($.False_kwd, $.True_kwd),

    False_kwd: $ => 'false',
    True_kwd: $ => 'true',
    wildcard: $ => '_',
    Let_kwd: $ => 'let',
    Const_kwd: $ => 'const',
  }
})
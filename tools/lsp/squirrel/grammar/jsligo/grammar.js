const common = require('../common.js')

module.exports = grammar({
  name: 'JsLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, /\s/],

  rules: {
    source_file: $ => 
      common.sepEndBy(optional(';'), field("toplevel", $._statement_or_namespace_or_preprocessor)),

    _statement_or_namespace_or_preprocessor: $ => choice($._statement, $.namespace_statement, $.preprocessor),

    namespace_statement: $ => seq(optional("export"), "namespace", field("moduleName", $.ModuleName), 
    '{',
      common.sepEndBy(";", $._statement_or_namespace_or_preprocessor), 
    '}'
    ),

    _statement: $ => prec.right(1, choice($._base_statement, $.if_statement)),

    if_statement: $ => seq("if", common.par($.expr), $._statement),

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

    break_statement: $ => "break",

    _expr_statement: $ => choice(
      $.fun_expr,
      $.type_as_annotation,
      $.assignment_operator,
      $.binary_operator,
      $.unary_operator,
      $._call_expr_level
    ),

    assignment_operator: $ => prec.right(2, seq($._expr_statement, choice("=", "*=", "/=", "%=", "+=", "-="), $._expr_statement)),

    type_as_annotation: $ => seq($._expr_statement, "as", $._core_type),

    binary_operator: $ => choice(
      prec.left(4, seq($._expr_statement, "||", $._expr_statement)),
      prec.left(5, seq($._expr_statement, "&&", $._expr_statement)),
      prec.left(10, seq($._expr_statement, choice("<", "<=", ">", ">=", "==", "!="), $._expr_statement)),
      prec.left(12, seq($._expr_statement, choice("+", "-"), $._expr_statement)),
      prec.left(13, seq($._expr_statement, choice("*", "/", "%"), $._expr_statement))
    ),

    unary_operator: $ => prec.right(15, seq(choice("-", "!"), $._expr_statement)),

    _call_expr_level: $ => prec.left(2, choice($._call_expr, $._member_expr)),

    _call_expr: $ => seq($.lambda, common.par(common.sepBy(",", $.expr))),

    lambda: $ => choice($._call_expr, $._member_expr),

    expr: $ => choice($._expr_statement, $.object_literal),

    _member_expr: $ => choice(
      $.Name,
      $.Int,
      $.Bytes,
      $.String,
      $.True,
      $.False,
      $.ctor_expr,
      $.projection,
      $.michelson_interop,
      $.paren_expr,
      $.module_access,
      $.array_literal,
      $.wildcard
    ),

    paren_expr: $ => common.par($.expr),

    ctor_expr: $ => seq($.ConstrName, $.ctor_args),

    ctor_args: $ => common.par(common.sepBy(",", $.expr)),

    projection: $ => choice(
      seq($._member_expr, common.brackets($.expr)),
      seq($._member_expr, ".", $.Name)
    ),

    michelson_interop: $ => seq(
      "(Michelson",
        seq(
          field("code", $.michelson_code),
          "as",
          field("type", $._core_type),
        ),
      ")"
    ),

    michelson_code: $ => seq('`', repeat(/([^\|]|\|[^}])/), '`'), // check ???

    module_access: $ => seq($.ModuleName, ".", $.module_var),

    module_var: $ => choice($.module_access, $.Name),

    array_literal: $ => common.brackets(common.sepBy1(",", $.array_item)),

    array_item: $ => choice($.expr, seq("...", $.expr)), 

    fun_expr: $ => choice(
      seq(common.par($.parameters), optional($.type_annotation), "=>", $.body),
      seq("(", ")", optional($.type_annotation), "=>", $.body),
      seq($.Name, "=>", $.body)
    ),

    body: $ => prec.right(3, choice(common.block($._statements), $._expr_statement)),

    _statements: $ => common.sepEndBy1(";", $._statement),

    type_annotation: $ => seq(":", $._type_expr),

    parameters: $ => common.sepBy1(",", $.parameter),

    parameter: $ => seq($.expr, $.type_annotation),

    return_statement: $ => prec.left(2, choice("return", seq("return", $.expr))),

    block_statement: $ => common.block($._statements),

    object_literal: $ => common.block(common.sepBy(",", $.property)),

    property: $ => choice($.Name, seq($.property_name, ":", $.expr), seq("...", $._expr_statement)),

    property_name: $ => choice($.Int, $.String, $.ConstrName, $.Name),

    _type_expr: $ => choice(
      $.fun_type,
      $.sum_type,
      $._core_type
    ),

    fun_type: $ => seq(common.par(common.sepBy(",", $.fun_param)), "=>", $._type_expr),

    fun_param: $ => seq($.Name, $.type_annotation),

    sum_type: $ => prec.right(1, common.withAttrs($, seq("|", common.sepBy("|", $.variant)))),

    variant: $ => common.withAttrs($, common.brackets(
      choice(
        seq('"', $.ConstrName, '"'),
        seq('"', $.ConstrName, '"', ",", common.sepBy(",", $._type_expr))
      )
    )),

    _core_type: $ => choice(
      $.String,
      $.Int,
      $.wildcard,
      $.TypeName,
      $.module_access_t,
      $.object_type,
      $.type_ctor_app,
      common.withAttrs($, $.type_tuple),
      common.par($._type_expr)
    ),

    module_access_t: $ => seq($.ModuleName, ".", $.module_var_t),

    module_var_t: $ => choice($.module_access_t, $.Name),

    object_type: $ => common.withAttrs($, common.block(common.sepEndBy(",", $.field_decl))),

    field_decl: $ => common.withAttrs($, choice(
      $.FieldName,
      seq($.FieldName, $.type_annotation)
    )),

    type_ctor_app: $ => prec(3, seq($.TypeName, common.chev(common.sepBy1(",", $._type_expr)))),

    type_tuple: $ => common.brackets(common.sepBy1(",", $._type_expr)),

    import_statement: $ => seq("import", $.ModuleName, "=", common.sepBy1(".", $.ModuleName)),

    export_statement: $ => seq("export", $._declaration_statement),

    _declaration_statement: $ => choice(
      $.let_decl,
      $.const_decl,
      $.type_decl
    ),

    type_decl: $ => seq("type", $.TypeName, optional($.type_params), "=", $._type_expr),

    type_params: $ => common.chev(common.sepBy1(",", $.TypeVariableName)),

    let_decl: $ => common.withAttrs($, seq("let", $._binding_list)),

    const_decl: $ => common.withAttrs($, seq("const", $._binding_list)),
    
    _binding_list: $ => common.sepBy1(",", $._binding_initializer),
    
    _binding_initializer: $ => seq($._binding_pattern, optional($.type_annotation), "=", $.expr),

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
      seq($.property_patterns, ",", $.property_pattern),
      seq($.property_patterns, ",", $.object_rest_pattern)
    ),

    property_pattern: $ => choice(
      seq($.Name, "=", $.expr),
      seq($.Name, ":", $._binding_initializer),
      $.var_pattern
    ),

    object_rest_pattern: $ => seq("...", $.Name),

    array_pattern: $ => common.brackets($._array_item_patterns),

    _array_item_patterns: $ => choice(
      $._array_item_pattern,
      seq($._array_item_patterns, ",", $._array_item_pattern),
      seq($._array_item_patterns, ",", $.array_rest_pattern)
    ),

    _array_item_pattern: $ => choice(
      $.var_pattern,
      $.wildcard,
      $.array_pattern
    ),

    array_rest_pattern: $ => seq("...", $.Name),

    switch_statement: $ => seq("switch", common.par($.expr), common.block($._cases)),

    _cases: $ => choice(
      seq(seq($.case, repeat($.case)), optional($.default_case)),
      $.default_case
    ),

    case: $ => seq("case", $.expr, $._case_statements),

    default_case: $ => seq("default", $._case_statements),

    _case_statements: $ => seq(":", choice(
      optional($._statements), 
      $.block_statement,
    )), 

    if_else_statement: $ => seq("if", common.par($.expr), $._base_statement, "else", $._statement),

    for_of_statement: $ => seq("for", common.par(seq($._index_kind, $.Name, "of", $._expr_statement)), $._statement),

    _index_kind: $ => choice($.Let_kwd, $.Const_kwd),
    
    while_statement: $ => seq("while", common.par($.expr), $._statement),

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
    _PositiveInt: $ => /([1-9][0-9_]*|0)/,
    Tez: $ => seq($._PositiveInt, "as", "tez"),
    Nat: $ => seq($._PositiveInt, "as", "nat"),
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
    wildcard: $ => '_',
    Let_kwd: $ => "let",
    Const_kwd: $ => "const", 
  }
});

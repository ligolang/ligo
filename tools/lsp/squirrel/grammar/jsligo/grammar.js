const common = require('../common.js')

module.exports = grammar({
  name: 'JsLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, /\s/],

  rules: {
    source_file: $ => choice(
      common.sepEndBy(optional(';'), field("toplevel", $.statement_or_namespace)),
      seq(field("toplevel", $.statement_or_namespace), optional(";"))
    ),

    statement_or_namespace: $ => choice($.statement, $.namespace_statement),

    namespace_statement: $ => seq(optional("export"), $.namespace),

    namespace: $ => seq("namespace", field("moduleName", $.ModuleName), common.block($.statements_or_namespace)),

    statements_or_namespace: $ => common.sepBy1(";", $.statement_or_namespace),

    statement: $ => prec.right(1, choice($.base_statement, $.if_statement)),

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
      seq($.assignment_expr_level, "as", $.core_type),
      $.disjunction_expr_level
    ),

    disjunction_expr_level: $ => choice(
      seq($.disjunction_expr_level, "||", $.conjunction_expr_level),
      $.conjunction_expr_level
    ),

    conjunction_expr_level: $ => choice(
      seq($.conjunction_expr_level, "&&", $.comparison_expr_level),
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

    addition_expr_level: $ => prec.left(2, choice(
      seq($.addition_expr_level, "+", $.multiplication_expr_level),
      seq($.addition_expr_level, "-", $.multiplication_expr_level),
      $.multiplication_expr_level
    )),

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

    call_expr_level: $ => prec.left(2, choice($.call_expr, $.member_expr)),

    call_expr: $ => seq($.lambda, common.par(common.sepBy(",", $.expr))),

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

    ctor_expr: $ => seq($.ConstrName, $.ctor_args),

    ctor_args: $ => common.par(common.sepBy(",", $.expr)),

    projection: $ => choice(
      seq($.member_expr, common.brackets($.expr)),
      seq($.member_expr, ".", $.Name)
    ),

    michelson_interop: $ => seq(
      "(Michelson",
        seq(
          field("code", $.michelson_code),
          "as",
          field("type", $.core_type),
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

    body: $ => choice(common.block($.statements), $.expr_statement),

    statements: $ => common.sepBy1(";", $.statement),

    type_annotation: $ => seq(":", $.type_expr),

    parameters: $ => common.sepBy1(",", $.parameter),

    parameter: $ => seq($.expr, $.type_annotation),

    return_statement: $ => prec.left(2, choice("return", seq("return", $.expr))),

    block_statement: $ => common.block($.statements),

    object_literal: $ => common.block(common.sepBy(",", $.property)),

    property: $ => choice($.Name, seq($.property_name, ":", $.expr), seq("...", $.expr_statement)),

    property_name: $ => choice($.Int, $.String, $.ConstrName, $.Name),

    type_expr: $ => choice(
      $.fun_type,
      $.sum_type,
      $.core_type
    ),

    fun_type: $ => seq(common.par(common.sepBy(",", $.fun_param)), "=>", $.type_expr),

    fun_param: $ => seq($.Name, $.type_annotation),

    sum_type: $ => prec.left(1, common.withAttrs($, seq("|", common.sepBy("|", $.variant)))),

    variant: $ => common.withAttrs($, common.brackets(
      choice(
        seq('"', $.ConstrName, '"'),
        seq('"', $.ConstrName, '"', ",", common.sepBy(",", $.type_expr))
      )
    )),

    core_type: $ => choice(
      $.String,
      $.Int,
      $.wildcard,
      $.TypeName,
      $.module_access_t,
      $.object_type,
      $.type_ctor_app,
      common.withAttrs($, $.type_tuple),
      common.par($.type_expr)
    ),

    module_access_t: $ => seq($.ModuleName, ".", $.module_var_t),

    module_var_t: $ => choice($.module_access_t, $.Name),

    object_type: $ => common.withAttrs($, common.block(common.sepEndBy(",", $.field_decl))),

    field_decl: $ => common.withAttrs($, choice(
      $.FieldName,
      seq($.FieldName, $.type_annotation)
    )),

    type_ctor_app: $ => seq($.TypeName, common.chev(common.sepBy1(",", $.type_expr))),

    type_tuple: $ => common.brackets(common.sepBy1(",", $.type_expr)),

    import_statement: $ => prec.left(1, seq("import", $.ModuleName, "=", common.sepBy(".", $.ModuleName))),

    export_statement: $ => seq("export", $.declaration_statement),

    declaration_statement: $ => choice(
      $.let_decl,
      $.const_decl,
      $.type_decl
    ),

    type_decl: $ => seq("type", $.TypeName, optional($.type_params), "=", $.type_expr),

    type_params: $ => common.chev(common.sepBy1(",", $.TypeVariableName)),

    let_decl: $ => common.withAttrs($, seq("let", $.binding_list)),

    const_decl: $ => common.withAttrs($, seq("const", $.binding_list)),
    
    binding_list: $ => common.sepBy1(",", $.binding_initializer),
    
    binding_initializer: $ => seq($.binding_pattern, optional($.type_annotation), "=", $.expr),

    binding_pattern: $ => choice(
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
      seq($.Name, ":", $.binding_initializer),
      $.var_pattern
    ),

    object_rest_pattern: $ => seq("...", $.Name),

    array_pattern: $ => common.brackets($.array_item_patterns),

    array_item_patterns: $ => choice(
      $.array_item_pattern,
      seq($.array_item_patterns, ",", $.array_item_pattern),
      seq($.array_item_patterns, ",", $.array_rest_pattern)
    ),

    array_item_pattern: $ => choice(
      $.var_pattern,
      $.wildcard,
      $.array_pattern
    ),

    array_rest_pattern: $ => seq("...", $.Name),

    switch_statement: $ => seq("switch", common.par($.expr), common.block($.cases)),

    cases: $ => choice(
      seq(seq($.case, repeat($.case)), optional($.default_case)),
      $.default_case
    ),

    case: $ => seq("case", $.expr, $.case_statements),

    default_case: $ => seq("default", $.case_statements),

    case_statements: $ => seq(":", optional(common.sepBy(";", $.case_statement))),

    case_statement: $ => choice(
      $.statement,
      "break"
    ),

    if_else_statement: $ => seq("if", common.par($.expr), $.base_statement, "else", $.statement),

    for_of_statement: $ => seq("for", common.par(seq($.index_kind, $.Name, "of", $.expr_statement)), $.statement),

    index_kind: $ => choice("let", "const"),
    
    while_statement: $ => seq("while", common.par($.expr), $.statement),

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
    wildcard: $ => '_',
  }
});

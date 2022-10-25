const common = require('../common.js')

const withAttrs = ($, x) => seq(field("attributes", repeat($.Attr)), x)

module.exports = grammar({
  name: 'JsLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker, $._js_ligo_attribute, $._automatic_semicolon],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, $._js_ligo_attribute, /\s/],

  conflicts: $ => [
    [$.variant],
    [$.Int, $.Nat, $.Tez],
    [$.Name, $.NameDecl],
    [$.tuple, $.list_pattern],
    [$.ConstrName, $.ModuleName],
    [$.ConstrNameType, $.String],
    [$.FieldName, $.NameDecl],
    [$.FieldName, $.Name]
  ],

  rules: {
    source_file: $ =>
      common.sepEndBy(optional($._semicolon), field("toplevel", $._toplevel)),

    _toplevel: $ => choice(
      seq(optional($._Expor_kwd), $.toplevel_binding),
      seq(optional($._Expor_kwd), $.type_decl),
      seq(optional($._Expor_kwd), $.namespace_statement),
      $.import_statement,
      $.preprocessor
    ),

    namespace_statement: $ => prec.left(2, seq(
      'namespace', field("moduleName", $.ModuleName),
      '{',
      common.sepEndBy($._semicolon, field("declaration", $._toplevel)),
      '}'
    )),

    _statement: $ => field("statement", $._base_statement),

    _base_statement: $ => prec(5, choice(
      $.let_binding,
      $.const_binding,
      $.type_decl,
      $._expr_statement,
      $.return_statement,
      $.block_statement,
      $.switch_statement,
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
      $.pattern_match,
      $._member_expr,
    ),

    list_literal: $ => seq('list', common.par($._list_elements)),

    assignment_operator: $ => prec.right(
      seq(field("lhs", $._expr),
      field("op", choice('=', '*=', '/=', '%=', '+=', '-=')),
      field("rhs", $._expr))
    ),

    type_as_annotation: $ => seq(field("subject", $._expr), 'as', field("type", $._core_type)),

    binary_call: $ => choice(
      prec.left(4,  seq(field("left", $._expr), field("op", '||'), field("right", $._expr))),
      prec.left(5,  seq(field("left", $._expr), field("op", '&&'), field("right", $._expr))),
      prec.left(10, seq(field("left", $._expr), field("op", choice('<', '<=', '>', '>=', '==', '!=')), field("right", $._expr))),
      prec.left(12, seq(field("left", $._expr), field("op", choice('+', '-')),      field("right", $._expr))),
      prec.left(13, seq(field("left", $._expr), field("op", choice('*', '/', '%')), field("right", $._expr)))
    ),

    unary_call: $ => prec.right(seq(field("negate", $.negate), field("arg", $._expr))),

    negate: $ => choice('-', '!'),

    apply: $ => prec.right(seq(field("function", $._apply), $._arguments)),

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
          field("subject", $._expr),
          ',',
          choice(
            seq('list',
              common.par(
                common.brackets(
                  common.sepBy1(',',
                    field("alt", $.list_case)
                  )
                )
              )
            ),
            $._ctor_cases
          )
        )
      )
    ),

    list_case: $ => seq(
      common.par(seq(field("pattern", $.list_pattern), ':', $._type_expr)),
      '=>',
      field("expr", $._body)
    ),

    _ctor_cases: $ => common.block(
      common.sepEndBy1(',',
        field("alt", $.ctor_case)
      )
    ),

    ctor_params: $ => common.sepBy1(',', field("ctor_param", $.ctor_param)),

    ctor_param: $ => seq(field("subject", $._binding_pattern), ':', field("type", $._type_expr)),

    ctor_case: $ => seq(
      field("pattern", $.constr_pattern),
      '=>',
      field("expr", $._body)
    ),

    constr_pattern: $ => seq(
      field("constructor", $.ConstrName),
      ':',
      common.par(optional(field("arg", $.ctor_params)))
    ),

    _member_expr: $ => choice(
      $.Name,
      $.Int,
      $.Bytes,
      $.String,
      $.Nat,
      $.Tez,
      $.False_kwd,
      $.True_kwd,
      $.Unit_kwd,
      $.Else_kwd, // Note: this is a hack to not allow broken if-else statements
      $.ConstrName,
      $.data_projection,
      $.indexing,
      $.code_inj,
      $.paren_expr,
      $.module_access,
      $.tuple,
      $.type_as_annotation,
    ),

    tuple: $ => common.brackets(common.sepBy(',', field("item", $._annot_expr))),

    paren_expr: $ => common.par(field("expr", $._expr)),

    data_projection: $ => prec.right(seq(field("expr", $._member_expr), '.', common.sepBy1('.', field("accessor", $.FieldName)))),

    indexing: $ => prec(5, seq(field("box", $._member_expr), common.brackets(field("index", $._expr)))),

    code_inj: $ => seq(
      field("lang", $.lang),
      field("code", $.Verbatim),
    ),

    module_access: $ => seq(
      common.sepBy1('.', field("path", $.ModuleName)),
      '.',
      field("field", $.Name),
    ),

    _list_elements: $ => common.brackets(common.sepBy(',', field("element", $._list_item))),

    _list_item: $ => choice($._annot_expr, $.spread),

    lambda: $ => choice(
      seq(
        common.par(common.sepBy1(',', field("argument", $.fun_arg))),
        optional(seq(':', field("type", $._type_expr))), '=>',
        field("body", $._body),
      ),
      seq(
        '(', ')',
        optional(seq(':', field("type", $._type_expr))), '=>',
        field("body", $._body),
      ),
      seq(
        field("argument", $.Name), '=>',
        field("body", $._body),
      )
    ),

    _body: $ => prec.right(
      choice(
        $.block_statement,
        $._expr
      )
    ),

    _statements: $ => common.sepEndBy1(optional($._semicolon), $._statement),

    _type_annotation: $ => seq(':', seq(optional($.type_params), field("type", $._type_expr))),

    fun_arg: $ => seq(field("argument", $._binding_pattern), optional(seq(':', optional($.type_params), field("type", $._type_expr)))),

    return_statement: $ => prec.right(seq('return', optional(field("expr", $._expr)))),

    block_statement: $ => prec.left(seq('{', $._statements, '}')),

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
      field("accessor", choice($.Int, $.String, $.ConstrName, $.FieldName)),
      ':',
      field("value", $._expr)
    ),

    capture: $ => field("accessor", $.FieldName),

    record_update: $ => common.block(seq(
      field("subject", $.spread),
      ',',
      common.sepEndBy1(',', field("field", $._record_field)),
    )),

    spread: $ => seq('...', field("name", $._expr)),

    _type_expr: $ => choice(
      $.fun_type,
      $.sum_type,
      $._core_type
    ),

    fun_type: $ => seq(field("domain", $.domain), '=>', field("codomain", $._type_expr)),

    domain: $ => common.par(common.sepBy(',', seq($._Name, ':', field("type", $._type_expr)))),

    sum_type: $ => withAttrs($, seq(optional('|'), common.sepBy1('|', field("variant", $.variant)))),

    variant: $ => withAttrs($, common.brackets(
      choice(
        field("constructor", $.ConstrNameType),
        seq(field("constructor", $.ConstrNameType), ',', field("ctor_arguments", $.ctor_arguments))
      )
    )),

    ctor_arguments: $ => common.sepBy1(',', field("ctor_argument", $._type_expr)),

    _core_type: $ => choice(
      $.Int,
      $.TypeWildcard,
      $.TypeName,
      $.string_type,
      $.module_access_t,
      $.record_type,
      $.disc_union_type,
      $.app_type,
      $.tuple_type,
      $.paren_type
    ),

    paren_type: $ => common.par(field("type", $._type_expr)),

    string_type: $ => field("value", $.String),

    module_access_t: $ => seq(common.sepBy1('.', field("path", $.ModuleName)), '.', field("type", $.TypeName)),

    record_type: $ => withAttrs($, common.block(common.sepEndBy(',', field("field_decl", $.field_decl)))),

    disc_union_type: $ => common.sepBy2('|', field("variant", $.record_type)),

    field_decl: $ => withAttrs($, choice(
      field("field_name", $.FieldName),
      seq(field("field_name", $.FieldName), ':', field("field_type", $._type_expr))
    )),

    app_type: $ => prec(3, seq(field("functor", $.TypeName), common.chev(common.sepBy1(',', field("argument", $._type_expr))))),

    tuple_type: $ => withAttrs($, common.brackets(common.sepBy1(',', field("element", $._type_expr)))),

    import_statement: $ => seq('import', field("moduleName", $.ModuleName), '=', common.sepBy1('.', field("module", $.ModuleName))),

    toplevel_binding:  $ => withAttrs($,
      seq(
        choice($._Let_kwd, $._Const_kwd),
        field("binding_pattern", $._binding_pattern),
        optional(
          seq(':', optional($.type_params), field("type_annot", $._type_expr))
        ),
        '=',
        field("value", $._expr)
      )
    ),

    let_binding: $ => withAttrs($,
      seq(
        $._Let_kwd,
        field("binding_pattern", $._binding_pattern),
        optional(
          seq(':', optional($.type_params), field("type_annot", $._type_expr))
        ),
        '=',
        field("value", $._expr)
      )
    ),

    const_binding: $ => withAttrs($,
      seq(
        $._Const_kwd,
        field("binding_pattern", $._binding_pattern),
        optional(
          seq(':', optional($.type_params), field("type_annot", $._type_expr))
        ),
        '=',
        field("value", $._expr)
      )
    ),

    type_decl: $ => seq("type", field("type_name", $.TypeName), optional(field("params", $.type_params)), '=', field("type_value", $._type_expr)),

    type_params: $ => common.chev(common.sepBy1(',', field("param", $.var_type))),

    var_type: $ => field("name", $.TypeVariableName),

    _binding_pattern: $ => prec(1,choice(
      $.var_pattern,
      $.wildcard,
      $.record_pattern,
      $.list_pattern,
      $.tuple_pattern,
      $.Unit_kwd
    )),

    var_pattern: $ => field("var", $.NameDecl),

    record_pattern: $ => withAttrs($, common.block(
      common.sepEndBy1(",", field("field", $._record_field_pattern)),
    )),

    _record_field_pattern: $ => choice(
      $.record_field_pattern,
      $.record_capture_pattern,
      $.record_rest_pattern,
    ),

    record_field_pattern: $ => withAttrs($, prec(9, seq(
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
      common.block(
        choice(
          seq(repeat1(field("case", $.case_statement)), optional(field("case", $.default_statement))),
          field("case", $.default_statement)
        )
      ),
      optional($._semicolon)
    )),

    case_statement: $ => seq('case', field("selector_value", $._expr), ':', optional(common.sepEndBy1($._semicolon, $._statement))),

    default_statement: $ => seq('default', ':', optional(common.sepEndBy1($._semicolon, $._statement))),

    if_else_statement: $ => prec.right(
      seq('if',
        common.par(field("selector", $._expr)),
        field("then_branch", $._base_statement),
        optional(seq('else', field("else_branch", $._base_statement)))
      )
    ),

    for_of_statement: $ => seq('for', common.par(seq($._index_kind, field("key", $.Name), 'of', field("collection", $._expr))), $._statement),

    _index_kind: $ => choice($._Let_kwd, $._Const_kwd),

    while_statement: $ => seq('while', common.par(field("breaker", $._expr)), $._statement),

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
    ConstrNameType: $ => /\"(\\.|[^"])+\"/,
    FieldName: $ => $._Name,
    ModuleName: $ => $._NameCapital,
    TypeName: $ => prec(1, choice($._Name, $._NameCapital)),
    TypeVariableName: $ => choice($._Name, $._NameCapital),
    Name: $ => $._Name,
    NameDecl: $ => $._Name,

    _till_newline: $ => /[^\n]*\n/,

    lang: $ => choice($._Name, $._NameCapital),
    Attr: $ => $._js_ligo_attribute,

    String: $ => /\"(\\.|[^"\n])*\"/,
    _Int: $ => /-?([1-9][0-9_]*|0)/,
    Int: $ => $._Int,
    Nat: $ => seq($._Int, 'as', 'nat'),
    Tez: $ => seq($._Int, 'as', choice('tez', 'mutez')),
    Verbatim: $ => seq('`', repeat(/([^\|]|\|[^}])/), '`'),
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,

    Unit_kwd: $ => 'unit',
    False_kwd: $ => 'false',
    True_kwd: $ => 'true',
    wildcard: $ => '_',
    _Let_kwd: $ => 'let',
    _Const_kwd: $ => 'const',
    _Expor_kwd: $ => 'export',
    Else_kwd: $ => 'else',
  }
})

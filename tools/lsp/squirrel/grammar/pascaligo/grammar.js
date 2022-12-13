const common = require('../common.js')

// Compound

const injection = (Kind, element) => seq(
  Kind,
  common.brackets(common.sepEndBy(';', element)),
)

// Instructions

const if_then_else_instr = ($, right_instr) => seq(
  'if',
  field("selector", $._expr),
  'then',
  field("then", $._test_clause_closed),
  'else',
  field("else", right_instr),
)

// Expressions

const if_then_else_expr = ($, right_expr) => seq(
  'if',
  field("selector", $._expr),
  'then',
  field("then", $._closed_expr),
  'else',
  field("else", right_expr),
)

const block_with = ($, right_expr) => seq(
  field("locals", $.block),
  'with',
  field("body", right_expr),
)

const fun_expr = ($, right_expr) => seq(
  'function',
  optional($._type_params),
  $.parameters,
  optional(seq(':', field("type", $._type_expr))),
  'is',
  field("body", right_expr),
)

// Case expression
const case_base = ($, case_clause_rhs) => seq(
  'case',
  field("subject", $._expr),
  'of',
  common.brackets(seq(
    optional('|'),
    common.sepBy1('|', field("case", case_clause_rhs)),
  )),
)

const case_clause_base = ($, rhs) => seq(
  field("pattern", $._pattern),
  '->',
  field("body", rhs),
)

module.exports = grammar({
  name: 'PascaLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment, $.line_marker],
  extras: $ => [$.ocaml_comment, $.comment, $.line_marker, /\s/],
  inline: $ => [$.parameters],

  rules: {
    source_file: $ => common.sepEndBy(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $.type_decl,
        $.const_decl,
        $.fun_decl,
        $.module_decl,
        $.module_alias,
        $.preprocessor,
      ),

    /// TYPE DECLARATION

    type_decl: $ =>
      seq(
        'type',
        field("typeName", $.TypeName),
        optional(field("params", $.type_vars)),
        'is',
        field("typeValue", $._type_expr),
      ),

    _type_params: $ => common.chev(
      common.sepBy1(',', field("param", $.var_type)),
    ),

    type_vars: $ => common.par(
      common.sepBy1(',', field("param", $.var_type)),
    ),

    _type_expr: $ => choice(
      $.sum_type,
      $.record_type,
      $._simple_type,
    ),

    // Sum type
    sum_type: $ => choice(
      common.sepBy1('|', field("variant", $.variant)),
      common.withAttrs($, seq('|', common.sepBy1('|', field("variant", $.variant)))),
    ),

    variant: $ => choice(
      common.withAttrs($, $._variant_simple),
      common.withAttrs($, $._variant_args),
    ),

    _variant_simple: $ => field("constructor", $.ConstrName),
    _variant_args: $ => seq(
      field("constructor", $.ConstrName),
      'of',
      field("arguments", $._param_type)
    ),

    // Record type
    record_type: $ => common.withAttrs($,
      choice(
        seq('record', common.sepEndBy(';', field("field", $.field_decl)), 'end'),
        seq('record', '[', common.sepEndBy(';', field("field", $.field_decl)), ']'),
      ),
    ),

    field_decl: $ => common.withAttrs($,
      seq(
        field("fieldName", $.FieldName),
        optional(seq(
          ':',
          field("fieldType", $._type_expr),
        )),
      ),
    ),

    // Simple type
    /* upstream: `fun_type` -> `cartesian` -> `core_type` */
    _simple_type: $ => choice(
      $.Int,
      $.TypeName,
      $.TypeWildcard,
      $.string_type,
      $.fun_type,
      $.prod_type,
      $.app_type,
      $.module_TypeName,
      $.type_group,
    ),

    var_type: $ => field("name", $.TypeVariableName),

    module_TypeName: $ =>
      seq(
        common.sepBy1('.', field("path", $.ModuleName)),
        '.',
        field("type", $.TypeName),
      ),

    fun_type: $ => prec.right(1, seq(
      field("domain", $._simple_type),
      '->',
      field("codomain", $._simple_type),
    )),

    prod_type: $ => prec.right(2,
      seq(
        field("element", $._simple_type),
        '*',
        field("element", $._simple_type),
      )
    ),

    string_type: $ => field("value", $.String),

    type_group: $ => common.par(field("type", $._type_expr)),

    app_type: $ => prec.left(8, seq(field("name", $._simple_type), $._type_arg)),

    _type_arg: $ => common.par(common.sepBy1(',', field("arg", $._type_expr))),

    /// CONSTANT DECLARATION

    const_decl: $ => common.withAttrs($,
      seq(
        'const',
        field("name", $._core_pattern),
        optional($._type_params),
        optional(seq(
          ':',
          field("type", $._type_expr),
        )),
        '=',
        field("value", $._expr),
      ),
    ),

    /// FUNCTION DECLARATION

    fun_decl: $ => common.withAttrs($,
      seq(
        field("recursive", optional($.recursive)),
        'function',
        field("name", $.NameDecl),
        optional($._type_params),
        $.parameters,
        optional(seq(':', field("type", $._type_expr))),
        'is',
        field("body", $._expr),
      ),
    ),

    parameters: $ => common.par(common.sepBy(';', field("parameter", $.param_decl))),

    _param_pattern: $ => choice(
      $.var_pattern,
      $.wildcard_pattern,
    ),

    param_decl: $ =>
      seq(
        field("access", $._access),
        field("name", $._param_pattern),
        ':',
        field("type", $._param_type),
      ),

    _access: $ => choice('var', 'const'),

    _param_type: $ => choice($._simple_type, $.record_type),

    /// MODULES

    module_decl: $ => choice(
      seq(
        "module",
        field("moduleName", $.ModuleName),
        "is",
        "{",
        common.sepEndBy(optional(';'), field("declaration", $._declaration)),
        "}"
      ),
      seq(
        "module",
        field("moduleName", $.ModuleName),
        "is",
        "begin",
        common.sepEndBy(optional(';'), field("declaration", $._declaration)),
        "end"
      ),
    ),


    module_alias: $ => seq(
      "module",
      field("moduleName", $.ModuleName),
      "is",
      common.sepBy1('.', field("module", $.ModuleName))
    ),

    /// STATEMENTS

    _statement: $ =>
      choice(
        $._open_data_decl,
        $._instruction,
      ),

    _open_data_decl: $ =>
      choice(
        $.type_decl,
        $.const_decl,
        $.var_decl,
        $.fun_decl,
        $.module_decl,
        $.module_alias,
      ),

    var_decl: $ =>
      seq(
        'var',
        field("name", $._pattern),
        optional($._type_params),
        optional(seq(':', field("type", $._type_expr))),
        ':=',
        field("value", $._expr),
      ),

    _instruction: $ => choice(
      $._base_instr,
      $.if_then_instr,
    ),
    _closed_instr: $ => $._base_instr_closed,

    _base_instr_common: $ => choice(
      $.patch_instr,
      $.remove_instr,
      $.case_instr,
      $._call_instr,
      $.for_int,
      $.for_in,
      $.while_loop,
      $.skip,
    ),
    _base_instr: $ => choice(
      $.if_then_else_instr,
      $.assignment,
      $._base_instr_common,
    ),
    _base_instr_closed: $ => choice(
      $.if_then_else_instr_closed,
      $.assignment_closed,
      $._base_instr_common,
    ),

    // Conditional Instruction
    if_then_else_instr: $ => if_then_else_instr($, $._test_clause),
    if_then_else_instr_closed: $ => if_then_else_instr($, $._test_clause_closed),
    if_then_instr: $ => seq(
      'if',
      field("selector", $._expr),
      'then',
      field("then", $._test_clause),
    ),

    _test_clause: $ => choice($._instruction, $.block),
    _test_clause_closed: $ => choice($._closed_instr, $.block),

    // Call instruction
    _call_instr: $ => $.call_expr,

    // Case instruction
    case_instr: $ => case_base($, $.case_clause_instr),
    case_clause_instr: $ => case_clause_base($, $._test_clause),

    // Assignment
    assignment: $ => seq(
      field("LHS", $._left_expr),
      ':=',
      field("RHS", $._expr),
    ),
    assignment_closed: $ => seq(
      field("LHS", $._left_expr),
      ':=',
      field("RHS", $._closed_expr),
    ),

    // Loops
    while_loop: $ =>
      seq(
        'while',
        field("breaker", $._expr),
        field("body", $.block),
      ),

    for_int: $ =>
      seq(
        'for',
        field("name", $.Name),
        ':=',
        field("begin", $._expr),
        'to',
        field("end", $._expr),
        optional(seq(
          "step",
          field("step", $._expr),
        )),
        field("body", $.block),
      ),

    for_in: $ =>
      seq(
        'for',
        field("key", $.Name),
        optional(seq('->', field("value", $.Name))),
        'in',
        field("kind", $.collection),
        field("collection", $._expr),
        field("body", $.block),
      ),

    collection: $ => choice('map', 'set', 'list'),
    removable: $ => choice('map', 'set'),

    // Patch instruction
    patch_instr: $ => seq(
      'patch',
      field("container", $._core_expr),
      'with',
      field("expr", $._patch_expr),
    ),

    _patch_expr: $ => choice(
      $.record_expr,
      $.map_expr,
      $.set_expr,
      $.patchable_call_expr,
      $.patchable_paren_expr,
    ),

    patchable_call_expr: $ => seq(
      field("patchable", $._patchable),
      field("expr", $.call_expr),
    ),

    patchable_paren_expr: $ => seq(
      field("patchable", $._patchable),
      field("expr", $.paren_expr),
    ),

    _patchable: $ => $.collection,

    // Remove instruction
    remove_instr: $ => seq(
      'remove',
      field("key", $._expr),
      'from',
      field("remove", $.removable),
      field("container", $._left_expr),
    ),

    /// PATTERNS

    _pattern: $ =>
      choice(
        $.cons_pattern,
        $._core_pattern,
      ),

    cons_pattern: $ =>
      seq(
        field("head", $._core_pattern),
        '#',
        field("tail", $._pattern),
      ),

    wildcard_pattern: $ => "_",

    _core_pattern: $ =>
      choice(
        $.wildcard_pattern,
        $.Int,
        $.Nat,
        $.Bytes,
        $.Verbatim,
        $.Tez,
        $.String,
        $.user_constr_pattern,
        $._list_pattern,
        $.record_pattern,
        $.tuple_pattern,
        $.var_pattern,
      ),

    user_constr_pattern: $ =>
      seq(
        field("constr", $.ConstrName),
        optional(field("arguments", $.tuple_pattern)),
      ),

    // List Pattern
    _list_pattern: $ =>
      choice(
        $.list_pattern,
        'nil',
      ),

    list_pattern: $ => injection("list", field("element", $._pattern)),

    // Record Pattern
    record_pattern: $ => injection("record", field("field", $._record_field_pattern)),

    _record_field_pattern: $ => choice(
      $.record_field_pattern,
      $.record_capture_pattern,
    ),

    record_field_pattern: $ => seq(
      field("name", $.FieldName),
      '=',
      field("body", $._core_pattern),
    ),

    record_capture_pattern: $ => field("name", $.NameDecl),

    // Tuple pattern
    tuple_pattern: $ =>
      common.par(common.sepBy1(',', field("element", $._pattern))),

    // Var pattern
    var_pattern: $ => field("name", $.NameDecl),

    /// EXPRESSIONS

    _expr: $ => choice(
      common.withAttrs($, $.if_then_expr),
      $._base_expr,
    ),
    _closed_expr: $ => choice(
      common.withAttrs($, $._attr_base_expr_closed),
      $._op_expr,
    ),
    _base_expr: $ => choice(
      common.withAttrs($, $._attr_base_expr),
      $._op_expr,
    ),
    _attr_base_expr: $ => choice(
      $.if_then_else_expr,
      $.block_with,
      $.fun_expr,
      $.case_expr,
    ),
    _attr_base_expr_closed: $ => choice(
      $.if_then_else_expr_closed,
      $.block_with_closed,
      $.fun_expr_closed,
      $.case_expr,
    ),

    _left_expr: $ => choice($.map_lookup, $._path_expr),

    // Case expressions
    case_expr: $ => case_base($, $.case_clause_expr),
    case_clause_expr: $ => case_clause_base($, $._expr),

    // Conditional expressions

    if_then_else_expr: $ => if_then_else_expr($, $._expr),
    if_then_else_expr_closed: $ => if_then_else_expr($, $._closed_expr),
    if_then_expr: $ => seq(
      'if',
      field("selector", $._expr),
      'then',
      field("then", $._expr),
    ),

    // Function expressions
    fun_expr: $ => fun_expr($, $._expr),
    fun_expr_closed: $ => fun_expr($, $._closed_expr),


    // Block expressions
    block_with: $ => block_with($, $._expr),
    block_with_closed: $ => block_with($, $._closed_expr),

    code_inj: $ => seq(
      "[%",
      // XXX: should be token.immediate($.Attr), but tree-sitter doesn't like it.
      field("lang", $.Attr),
      field("code", $._expr),
      ']',
    ),

    // Operation expressions
    _op_expr: $ =>
      choice(
        $.binop,
        $.unop,
        $._core_expr,
      ),

    binop: $ =>
      choice(
        prec.left(0, seq(field("arg1", $._op_expr), field("op", 'or'), field("arg2", $._op_expr))),
        prec.left(1, seq(field("arg1", $._op_expr), field("op", 'and'), field("arg2", $._op_expr))),
        prec.right(2, seq(field("arg1", $._core_expr), field("op", 'contains'), field("arg2", $._op_expr))),
        prec.left(3, seq(field("arg1", $._op_expr), field("op", $.comparison), field("arg2", $._op_expr))),
        prec.right(4, seq(field("arg1", $._op_expr), field("op", '^'), field("arg2", $._op_expr))),
        prec.right(5, seq(field("arg1", $._op_expr), field("op", '#'), field("arg2", $._op_expr))),
        prec.left(6, seq(field("arg1", $._op_expr), field("op", $.adder), field("arg2", $._op_expr))),
        prec.left(7, seq(field("arg1", $._op_expr), field("op", $.multiplier), field("arg2", $._op_expr))),
      ),

    comparison: $ => choice('<', '<=', '>', '>=', '=', '=/='),
    adder: $ => choice('-', '+'),
    multiplier: $ => choice('/', '*', 'mod'),

    unop: $ => prec.right(8, seq(field("negate", $.negate), field("arg", $._core_expr))),

    negate: $ => choice('-', 'not'),

    /// CORE EXPRESSIONS

    _core_expr: $ =>
      choice(
        $.Int,
        $.Nat,
        $.Tez,
        $.String,
        $.Verbatim,
        $.Bytes,

        $.annot_expr,
        $.tuple_expr,
        $._list_expr,
        $.map_expr,
        $.big_map_expr,
        $.set_expr,
        $.record_expr,
        $.code_inj,
        $.update_record,
        $._ctor_app_expr,
        $.call_expr,
        $._left_expr,
      ),

    // Annotation expression
    annot_expr: $ =>
      common.par(seq(
        field("subject", $._op_expr),
        ':',
        field("type", $._type_expr)
      )),

    // Tuple expression
    tuple_expr: $ => common.par(seq(
      field("element", $._expr),
      ',',
      common.sepBy1(',', field("element", $._expr))),
    ),

    // List expression
    _list_expr: $ => choice($.list_injection, 'nil'),

    list_injection: $ => injection('list', field("element", $._expr)),

    // Call expression
    call_expr: $ => seq(
      field("f", $._path_expr),
      common.par(common.sepBy(',', field("argument", $._expr))),
    ),

    // (Big) Map expression
    map_expr: $ => injection('map', field("binding", $.binding)),
    big_map_expr: $ => injection('big_map', field("binding", $.binding)),

    map_lookup: $ =>
      seq(
        field("container", $._path_expr),
        repeat1(common.brackets(field("index", $._expr))),
      ),

    // Set Expression
    set_expr: $ => injection('set', field("element", $._expr)),

    // Record Expression
    record_expr: $ =>
      injection('record', field("assignment", $.field_path_assignment)),

    // Update Record Expression
    update_record: $ =>
      seq(
        field("record", $._core_expr),
        'with',
        injection('record', field("assignment", $.field_path_assignment)),
      ),

    // Constructor application expression
    _ctor_app_expr: $ => choice(
      $.ctor_app_expr,
      $.ConstrName,
    ),

    ctor_app_expr: $ => seq(
      field("ctor", $.ConstrName),
      common.par(common.sepBy1(',', field("arguments", $._expr))),
    ),

    // Paren expression
    paren_expr: $ => common.par(field("expr", $._expr)),

    /// Path expressions

    _path_expr: $ => choice(
      $.module_access,
      $._local_path,
    ),

    _local_path: $ => choice(
      $.data_projection,
      $._field_path,
    ),

    data_projection: $ => seq(
      field("selector", $.paren_expr),
      '.',
      $._accessor_chain,
    ),

    _field_path: $ => choice(
      $.field_path,
      $.paren_expr,
      $.Name,
    ),

    field_path: $ => seq(
      field("selector", $.Name),
      '.',
      $._accessor_chain,
    ),

    _accessor_chain: $ => prec.right(common.sepBy1('.', field("accessor", $._accessor))),
    _accessor: $ => choice($.FieldName, $.Int),

    module_access: $ => seq(
      common.sepBy1('.', field("path", $.ModuleName)),
      '.',
      field("field", $._field_path),
    ),

    /// PREPROCESSOR

    // I (@heitor.toledo) decided to keep the preprocessors here since we still
    // attempt to parse the contract even if `ligo preprocess` failed.
    preprocessor: $ => field("preprocessor_command", choice(
      $.p_include,
      $.p_import,
      $.p_if,
      $.p_error,
      $.p_define,
    )),

    p_include: $ => seq(
      /#\s*include/,
      field("filename", $.String)
    ),

    p_import: $ => seq(
      /#\s*import/,
      field("filename", $.String),
      field("alias", $.String),
    ),

    p_if: $ => choice(
      seq(
        /#\s*(if|elif|else)/,
        field("rest", $._till_newline),
      ),
      /#\s*endif/,
    ),

    p_error: $ => seq(/#\s*error/, field("message", $._till_newline)),
    p_define: $ => seq(/#\s*(define|undef)/, field("definition", $._till_newline)),

    /// MISCELLANEOUS UTILITIES

    block: $ => choice(
      seq(
        'begin',
        common.sepEndBy(';', field("statement", $._statement)),
        'end',
      ),
      seq(
        optional('block'),
        '{',
        common.sepEndBy(';', field("statement", $._statement)),
        '}',
      )
    ),

    binding: $ =>
     seq(
        field("key", $._expr),
        '->',
        field("value", $._expr),
      ),

    field_path_assignment: $ =>
      seq(
        $._accessor_chain,
        '=',
        field("_rhs", $._expr),
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

    String: $ => choice(/\"(\\.|[^"\n])*\"/, /{\|(\\.|[^\|])*\|}/),
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Verbatim: $ => seq('{|', repeat(/([^\|]|\|[^}])/), '|}'),
    Bytes: $ => /0x[0-9a-fA-F]+/,

    _Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    NameWildcard: $ => '_',
    TypeWildcard: $ => '_',
    Keyword: $ => /[A-Za-z][a-z]*/,

    skip: $ => 'skip',
    recursive: $ => 'recursive',
  }
});

let sepBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)))
let sepBy = (sep, rule) => optional(sepBy1(sep, rule))

let sepEndBy1 = (sep, rule) => seq(rule, repeat(seq(sep, rule)), optional(sep))
let sepEndBy = (sep, rule) => optional(sepEndBy1(sep, rule))

let op = (name, left, right, term) =>
  choice(
    seq(left, name, right),
    term,
  )

let right_op = (name, left, right) => op(name, left, right, right)
let left_op = (name, left, right) => op(name, left, right, left)

let par = x => seq('(', x, ')')
// Don't consider parens as a part of a subnode, this little hack
// allows us to skip nested parens in product types.
let brackets = x => seq('[', x, ']')

let withAttrs = ($, x) => seq(field("attributes", repeat($.attr)), x)

let non_empty_injection = (Kind, element) =>
  choice(
    seq(
      Kind,
      sepEndBy1(';', element),
      'end',
    ),
    seq(
      Kind,
      '[',
      sepEndBy1(';', element),
      ']',
    ),
  )

let injection = (Kind, element) =>
  choice(
    seq(
      Kind,
      sepEndBy(';', element),
      'end',
    ),
    seq(
      Kind,
      '[',
      sepEndBy(';', element),
      ']',
    ),
  )

module.exports = grammar({
  name: 'PascaLigo',

  word: $ => $.Keyword,
  externals: $ => [$.ocaml_comment, $.comment],
  extras: $ => [$.ocaml_comment, $.comment, /\s/],
  inline: $ => [$.parameters, $.arguments],

  rules: {
    source_file: $ => sepEndBy(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $.type_decl,
        $.const_decl,
        $.fun_decl,
        $.preprocessor,
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
      optional(par(sepBy(',', field("argument", $._expr)))),
      ']'
    ),

    type_decl: $ =>
      seq(
        "type",
        field("typeName", $.TypeName),
        "is",
        field("typeValue", $._type_expr),
      ),

    module_TypeName: $ =>
      seq(
        sepBy1('.', field("path", $.NameModule)),
        '.',
        field("type", $.TypeName),
      ),

    _type_expr: $ => choice(
      $.sum_type,
      $.record_type,
      $._simple_type,
    ),

    // upstream: `fun_type` -> `cartesian` -> `core_type`
    _simple_type: $ => choice(
      $.module_TypeName,
      $.fun_type,
      $.prod_type,
      $.TypeName,
      $.TypeWildcard,
      $.type_group,
      $.app_type,
      $.michelsonTypeOr,
      $.michelsonTypeAnd,
      $.Int,
    ),

    type_group: $ => par(field("type", $._type_expr)),

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

    app_type: $ => prec.left(8, seq(field("name", $._simple_type), $._type_arg)),

    _type_arg: $ => par(sepBy1(',', field("arg", $._type_expr))),

    michelsonTypeOr: $ =>
      seq(
        "michelson_or",
        "(",
        field("left_type", $._type_expr),
        ",",
        field("left_type_name", $.String),
        ",",
        field("right_type", $._type_expr),
        ",",
        field("right_type_name", $.String),
        ")",
      ),

    michelsonTypeAnd: $ =>
      seq(
        "michelson_pair",
        "(",
        field("left_type", $._type_expr),
        ",",
        field("left_type_name", $.String),
        ",",
        field("right_type", $._type_expr),
        ",",
        field("right_type_name", $.String),
        ")",
      ),

    sum_type: $ => choice(
      sepBy1('|', field("variant", $.variant)),
      withAttrs($, seq('|', sepBy1('|', field("variant", $.variant)))),
    ),

    variant: $ => choice(
      withAttrs($, $._variant_simple),
      withAttrs($, $._variant_args),
    ),

    _variant_simple: $ => field("constructor", $.NameConstr),
    _variant_args: $ => seq(
      field("constructor", $.NameConstr),
      'of',
      field("arguments", $._simple_type)
    ),

    record_type: $ => withAttrs($,
      choice(
        seq('record', sepEndBy(';', field("field", $.field_decl)), 'end'),
        seq('record', '[', sepEndBy(';', field("field", $.field_decl)), ']'),
      ),
    ),

    field_decl: $ => withAttrs($,
      seq(
        field("fieldName", $.FieldName),
        ':',
        field("fieldType", $._type_expr),
      ),
    ),

    fun_expr: $ =>
      seq(
        field("recursive", optional($.recursive)),
        'function',
        $.parameters,
        optional(seq(':', field("type", $._type_expr))),
        'is',
        field("body", $._expr),
      ),

    fun_decl: $ => withAttrs($,
      seq(
        field("recursive", optional($.recursive)),
        'function',
        field("name", $.NameDecl),
        $.parameters,
        optional(seq(':', field("type", $._type_expr))),
        'is',
        field("body", $._expr),
      ),
    ),

    let_expr: $ =>
      seq(
        field("locals", $.block),
        'with',
        field("body", $._expr),
      ),

    parameters: $ => par(sepBy(';', field("parameter", $.param_decl))),

    param_decl: $ =>
      seq(
        field("access", $._access),
        field("name", choice($.NameDecl, $.NameWildcard)),
        ':',
        field("type", $._param_type),
      ),

    _access: $ => choice('var', 'const'),

    _param_type: $ => $._simple_type,

    _statement: $ =>
      choice(
        $._instruction,
        $._open_data_decl,
        $.type_decl,
      ),

    _open_data_decl: $ =>
      choice(
        $.const_decl,
        $.var_decl,
        $.fun_decl,
      ),

    const_decl: $ => withAttrs($,
      seq(
        'const',
        field("name", $.NameDecl),
        optional(seq(
          ':',
          field("type", $._type_expr),
        )),
        '=',
        field("value", $._expr),
      ),
    ),

    var_decl: $ =>
      seq(
        'var',
        field("name", $._pattern),
        optional(seq(':', field("type", $._type_expr))),
        ':=',
        field("value", $._expr),
      ),

    _instruction: $ =>
      choice(
        $.conditional,
        $.case_instr,
        $.assignment,
        $._loop,
        $.fun_call,
        $.projection_call,
        $.skip,
        $.record_patch,
        $.map_patch,
        $.set_patch,
        $.map_remove,
        $.set_remove,
      ),

    set_remove: $ =>
      seq(
        'remove',
        field("key", $._expr),
        'from',
        'set',
        field("container", $._path),
      ),

    map_remove: $ =>
      seq(
        'remove',
        field("key", $._expr),
        'from',
        'map',
        field("container", $._path),
      ),

    set_patch: $ =>
      seq(
        'patch',
        field("container", $._path),
        'with',
        non_empty_injection('set', field("key", $._expr)),
      ),

    map_patch: $ =>
      seq(
        'patch',
        field("container", $._path),
        'with',
        non_empty_injection('map', field("binding", $.binding)),
      ),

    binding: $ =>
      seq(
        field("key", $._expr),
        '->',
        field("value", $._expr),
      ),

    record_patch: $ =>
      seq(
        'patch',
        field("container", $._path),
        'with',
        non_empty_injection('record', field("binding", $.field_path_assignment)),
      ),

    conditional: $ =>
      seq(
        'if',
        field("selector", $._expr),
        'then',
        field("then", $._if_clause),
        optional(';'),
        'else',
        field("else", $._if_clause),
      ),

    _if_clause: $ =>
      choice(
        $._instruction,
        $.clause_block,
        $.block,
      ),

    clause_block: $ =>
      seq('{', sepEndBy1(';', field("statement", $._statement)), '}'),

    block: $ =>
      choice(
        seq(
          'begin',
          sepEndBy(';', field("statement", $._statement)),
          'end',
        ),
        seq(
          'block',
          '{',
          sepEndBy(';', field("statement", $._statement)),
          '}',
        ),
      ),

    case_instr: $ =>
      choice(
        seq(
          'case',
          field("subject", $._expr),
          'of',
          optional('|'),
          sepEndBy1('|', field("case", $.case_clause_instr)),
          'end'
        ),
        seq(
          'case',
          $._expr,
          'of',
          '[',
          optional('|'),
          sepEndBy1('|', field("case", $.case_clause_instr)),
          ']'
        ),
      ),

    case_clause_instr: $ =>
      seq(
        field("pattern", $._pattern),
        '->',
        field("body", $._if_clause),
      ),

    assignment: $ =>
      seq(
        field("LHS", $._lhs),
        ':=',
        field("RHS", $._rhs),
      ),

    _rhs: $ => $._expr,
    _lhs: $ => choice($._path, $.map_lookup),

    _loop: $ => choice($.while_loop, $._for_loop),

    while_loop: $ =>
      seq(
        'while',
        field("breaker", $._expr),
        field("body", $.block),
      ),

    _for_loop: $ =>
      choice(
        $.for_cycle,
        $.for_box,
      ),

    for_cycle: $ =>
      seq(
        'for',
        field("name", $.Name),
        ':=',
        field("begin", $._rhs),
        'to',
        field("end", $._expr),
        optional(seq(
          "step",
          field("step", $._expr),
        )),
        field("body", $.block),
      ),

    for_box: $ =>
      seq(
        'for',
        field("key", $.Name),
        optional(seq('->', field("value", $.Name))),
        'in',
        field("kind", $._collection),
        field("collection", $._expr),
        field("body", $.block),
      ),

    _collection: $ => choice('map', 'set', 'list'),

    _expr: $ =>
      choice(
        $.case_expr,
        $.cond_expr,
        $._op_expr,
        $.fun_expr,
        $.michelson_interop,
        $.let_expr,
      ),

    case_expr: $ =>
      choice(
        seq(
          'case',
          field("subject", $._expr),
          'of',
          optional('|'),
          sepEndBy1('|', field("case", $.case_clause_expr)),
          'end'
        ),
        seq(
          'case',
          field("subject", $._expr),
          'of',
          '[',
          optional('|'),
          sepEndBy1('|', field("case", $.case_clause_expr)),
          ']'
        ),
      ),

    case_clause_expr: $ =>
      seq(
        field("pattern", $._pattern),
        '->',
        field("body", $._expr),
      ),

    cond_expr: $ =>
      seq(
        'if',
        field("selector", $._expr),
        'then',
        field("then", $._expr),
        optional(';'),
        'else',
        field("else", $._expr),
      ),

    _op_expr: $ =>
      choice(
        $._core_expr,
        $.binop,
        $.unop,
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

    unop: $ => prec.right(8, seq(field("negate", $.negate), field("arg", $._core_expr))),

    comparison: $ => choice('<', '<=', '>', '>=', '=', '=/='),
    adder: $ => choice('-', '+'),
    multiplier: $ => choice('/', '*', 'mod'),
    negate: $ => choice('-', 'not'),

    _core_expr: $ =>
      choice(
        $.Int,
        $.Nat,
        $.Tez,
        $.Name,
        $.String,
        $.Bytes,
        $.False,
        $.True,
        $.Unit,
        $.annot_expr,
        $.tuple_expr,
        $._list_expr,
        $.None,
        $._fun_call_or_par_or_projection,
        $._map_expr,
        $.set_expr,
        $.record_expr,
        $.update_record,
        $._constr_use,
        $.paren_expr,
      ),

    _constr_use: $ =>
      choice(
        $.constr_call,
        $.NameConstr
      ),

    constr_call: $ =>
      seq(
        field("constr", $.NameConstr),
        $.arguments
      ),

    Some_call: $ =>
      seq(
        field("constr", 'Some'),
        $.arguments,
      ),

    _fun_call_or_par_or_projection: $ =>
      choice(
        $.par_call,
        $.projection_call,
        $.fun_call,
        $._projection,
      ),

    par_call: $ =>
      prec.right(1, seq(
        par(field("f", $._expr)),
        $.arguments,
      )),

    projection_call: $ => prec(1, seq(
      field("f", $._projection),
      $.arguments,
    )),

    annot_expr: $ =>
      par(seq(
        field("subject", $._op_expr),
        ':',
        field("type", $._type_expr)
      )),

    set_expr: $ => injection('set', field("element", $._expr)),

    _map_expr: $ =>
      choice(
        $.map_lookup,
        $.map_injection,
        $.big_map_injection,
      ),

    map_injection: $ => injection('map', field("binding", $.binding)),
    big_map_injection: $ => injection('big_map', field("binding", $.binding)),

    map_lookup: $ =>
      seq(
        field("container", $._path),
        brackets(field("index", $._expr)),
      ),

    module_access: $ => seq(
      sepBy1('.', field("path", $.NameModule)),
      '.',
      field("field", $.FieldName),
    ),

    _path: $ => choice($.Name, $._projection),

    _accessor: $ => choice($.FieldName, $.Int),

    // field names (or indices) separated by a dot
    _accessor_chain: $ => prec.right(sepBy1('.', field("accessor", $._accessor))),

    _projection: $ =>
      choice(
        $.data_projection,
        $.module_access,
      ),

    data_projection: $ => prec(11, seq(
      field("struct", $.Name),
      '.',
      $._accessor_chain,
    )),

    record_expr: $ =>
      injection('record', field("assignment", $.field_path_assignment)),

    update_record: $ =>
      seq(
        field("record", $._path),
        'with',
        non_empty_injection('record', field("assignment", $.field_path_assignment)),
      ),

    field_path_assignment: $ =>
      seq(
        $._accessor_chain,
        '=',
        field("_rhs", $._expr),
      ),

    fun_call: $ =>
      seq(
        field("f", choice($.Name)),
        $.arguments,
      ),

    paren_expr: $ => par(field("expr", $._expr)),

    tuple_expr: $ => par(seq(
      field("element", $._expr),
      ',',
      sepBy1(',', field("element", $._expr))),
    ),
    arguments: $ => par(sepBy(',', field("argument", $._expr))),

    _list_expr: $ => choice($.list_injection, 'nil'),

    list_injection: $ => injection('list', field("element", $._expr)),

    _pattern: $ =>
      choice(
        $.cons_pattern,
        $._core_pattern,
      ),

    _core_pattern: $ =>
      choice(
        $.var_pattern,
        '_',
        $.Int,
        $.Nat,
        $.String,
        $._list_pattern,
        $.tuple_pattern,
        $._constr_pattern,
        $.record_pattern,
      ),

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

    var_pattern: $ => field("name", $.NameDecl),

    _list_pattern: $ =>
      choice(
        $.list_pattern,
        'nil',
      ),

    list_pattern: $ => injection("list", field("element", $._pattern)),

    cons_pattern: $ =>
      seq(
        field("head", $._core_pattern),
        '#',
        field("tail", $._pattern),
      ),

    tuple_pattern: $ =>
      par(sepBy1(',', field("element", $._pattern))),

    _constr_pattern: $ => choice(
      $.Unit,
      $.False,
      $.True,
      $.None,
      $.user_constr_pattern,
    ),

    user_constr_pattern: $ =>
      seq(
        field("constr", $.NameConstr),
        optional(field("arguments", $.tuple_pattern)),
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

    _till_newline: $ => /[^\n]*\n/,

    attr: $ => /\[@[a-zA-Z][a-zA-Z0-9_:]*\]/,

    _NameCapital: $ => /[A-Z][a-zA-Z0-9_]*/,
    String: $ => choice(/\"(\\.|[^"])*\"/, /{\|(\\.|[^\|])*\|}/),
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,
    FieldName: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    TypeName: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    TypeWildcard: $ => '_',
    Name: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    NameDecl: $ => /[a-z][a-zA-Z0-9_]*|_(?:_?[a-zA-Z0-9])+/,
    NameWildcard: $ => '_',
    NameConstr: $ => $._NameCapital,
    NameModule: $ => $._NameCapital,
    Keyword: $ => /[A-Za-z][a-z]*/,

    False: $ => 'False',
    True: $ => 'True',
    Unit: $ => 'Unit',
    None: $ => 'None',
    skip: $ => 'skip',
    recursive: $ => 'recursive',
  }
});

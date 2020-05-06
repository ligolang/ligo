let sepBy1 = (sep, rule) =>
  seq(
    rule,
    repeat(seq(sep, rule)),
    optional(sep)
  )

let sepBy = (sep, rule) => optional(sepBy1(sep, rule))

let op = (name, left, right, term) =>
  choice(
    seq(left, name, right),
    term,
  )

let right_op = (name, left, right) => op(name, left, right, right)
let left_op  = (name, left, right) => op(name, left, right, left)

let par = x => seq('(', x, ')')
let brackets = x => seq('[', x, ']')

let ne_injection = (Kind, element) =>
  choice(
    seq(
      Kind,
      sepBy1(';', element),
      'end',
    ),
    seq(
      Kind,
      '[',
      sepBy1(';', element),
      ']',
    ),
  )

let injection = (Kind, element) =>
  choice(
    seq(
      Kind,
      sepBy(';', element),
      'end',
    ),
    seq(
      Kind,
      '[',
      sepBy(';', element),
      ']',
    ),
  )

module.exports = grammar({
  name: 'PascaLigo',

  word:   $ => $.Keyword,
  extras: $ => [$.ocaml_comment, $.comment, /\s/],

  rules: {
    contract: $ => sepBy(optional(';'), field("declaration", $._declaration)),

    _declaration: $ =>
      choice(
        $.type_decl,
        $.const_decl,
        $.fun_decl,
        $.attr_decl,
        $.include,
      ),

    attr_decl: $ =>
      injection("attributes",
        field("attribute", $.String)
      ),

    type_decl: $ =>
      seq(
        "type",
        field("typeName", $.Name),
        "is",
        field("typeValue", $._type_expr),
      ),

    type_expr : $ => $._type_expr,

    _type_expr: $ =>
      choice(
        $.fun_type,
        $.sum_type,
        $.record_type,
      ),

    fun_type: $ =>
      choice(
        field("domain", $.cartesian),
        seq(
          field("domain", $.cartesian),
          '->',
          field("codomain", $.fun_type),
        ),
      ),

    cartesian: $ =>
      sepBy1('*', field("element", $._core_type)),

    _core_type: $ =>
      choice(
        $.Name,
        par($.type_expr),
        $.invokeBinary,
        $.invokeUnary,
      ),

    invokeBinary: $ =>
      seq(
        field("typeConstr", choice('map', 'big_map', $.Name)),
        field("arguments", $.type_tuple),
      ),

    invokeUnary: $ =>
      seq(
        field("typeConstr", choice('list', 'set')),
        par(field("arguments", $._type_expr)),
      ),

    map:     $ => 'map',
    big_map: $ => 'big_map',
    list:    $ => 'list',
    set:     $ => 'set',

    type_tuple: $ => par(sepBy1(',', field("element", $._type_expr))),

    sum_type: $ =>
      seq(
        optional('|'),
        sepBy1('|', field("variant", $.variant)),
      ),

    variant: $ =>
      choice(
        field("constructor", $.constr),
        seq(
          field("constructor", $.constr),
          'of',
          field("arguments", $.fun_type)
        ),
      ),

    constr: $ => $.Name_Capital,

    record_type: $ =>
      choice(
        seq('record',      sepBy(';', field("field", $.field_decl)), 'end'),
        seq('record', '[', sepBy(';', field("field", $.field_decl)), ']'),
      ),

    field_decl: $ =>
      seq(
        field("fieldName", $.Name),
        ':',
        field("fieldType", $._type_expr),
      ),

    fun_expr: $ =>
      seq(
        field("recursive", optional($.recursive)),
        'function',
        field("parameters", $.parameters),
        ':',
        field("type", $._type_expr),
        'is',
        field("body", $._expr),
      ),

    fun_decl: $ =>
      prec.right(0,
        seq(
          field("recursive", optional($.recursive)),
          'function',
          field("name", $.Name),
          field("parameters", $.parameters),
          ':',
          field("type", $._type_expr),
          'is',
          field("body", $.let_expr),
        ),
      ),

    let_expr: $ =>
      choice(
        seq(
          field("locals", $.block),
          'with',
          field("body", $._expr),
        ),
        field("body", $._expr),
      ),

    parameters: $ => par(sepBy(';', field("parameter", $.param_decl))),

    param_decl: $ =>
      seq(
        field("access", $.access),
        field("name", $.Name),
        ':',
        field("type", $._param_type),
      ),

    access: $ => choice('var', 'const'),

    _param_type: $ => $.fun_type,

    _statement: $ =>
      choice(
        $._instruction,
        $._open_data_decl,
        $.attr_decl,
      ),

    _open_data_decl: $ =>
      choice(
        $.const_decl,
        $.var_decl,
        $.fun_decl,
      ),

    const_decl: $ =>
      seq(
        'const',
        field("name", $.Name),
        ':',
        field("type", $._type_expr),
        '=',
        field("value", $._expr),
      ),

    var_decl: $ =>
      seq(
        'var',
        field("name", $.Name),
        ':',
        field("type", $._type_expr),
        ':=',
        field("value", $._expr),
      ),

    _instruction: $ =>
      choice(
        $.conditional,
        $.case_instr,
        $.assignment,
        $.loop,
        $._proc_call,
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
        field("container", $.path),
      ),

    map_remove: $ =>
      seq(
        'remove',
        field("key", $._expr),
        'from',
        'map',
        field("container", $.path),
      ),

    set_patch: $ =>
      seq(
        'patch',
        field("container", $.path),
        'with',
        ne_injection('set', field("key", $._expr)),
      ),

    map_patch: $ =>
      seq(
        'patch',
        field("container", $.path),
        'with',
        ne_injection('map', field("binding", $.binding)),
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
        field("container", $.path),
        'with',
        ne_injection('record', field("binding", $.field_assignment)),
      ),

    _proc_call: $ =>
      $.fun_call,

    conditional: $ =>
      seq(
        'if',
        field("selector", $._expr),
        'then',
        field("then", $.if_clause),
        optional(';'),
        'else',
        field("else", $.if_clause),
      ),

    if_clause: $ =>
      choice(
        $._instruction,
        $.clause_block,
      ),

    clause_block: $ =>
      choice(
        field("block", $.block),
        seq('{', sepBy1(';', field("statement", $._statement)), '}')
      ),

    block: $ =>
      choice(
        seq(
          'begin',
          sepBy(';', field("statement", $._statement)),
          'end',
        ),
        seq(
          'block',
          '{',
          sepBy(';', field("statement", $._statement)),
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
          sepBy1('|', field("case", $.case_clause_instr)),
          'end'
        ),
        seq(
          'case',
          $._expr,
          'of',
          '[',
          optional('|'),
          sepBy1('|', field("case", $.case_clause_instr)),
          ']'
        ),
      ),

    case_clause_instr: $ =>
      seq(
        field("pattern", $.pattern),
        '->',
        field("body", $.if_clause),
      ),

    assignment: $ =>
      seq(
        field("LHS", $._lhs),
        ':=',
        field("RHS", $._rhs),
      ),

    _rhs: $ => $._expr,
    _lhs: $ => choice($.path, $.map_lookup),

    loop: $ => choice($.while_loop, $.for_loop),

    while_loop: $ =>
      seq(
        'while',
        field("breaker", $._expr),
        field("body", $.block),
      ),

    for_loop: $ =>
      choice(
        seq(
          'for',
          field("name", $.Name),
          ':=',
          field("begin", $._rhs),
          'to',
          field("end", $._expr),
          field("body", $.block),
        ),
        seq(
          'for',
          field("key", $.Name),
          optional(seq('->', field("value", $.Name))),
          'in',
          field("kind", $.collection),
          field("collection", $._expr),
          field("body", $.block),
        ),
      ),

    collection: $ => choice('map', 'set', 'list'),

    interactive_expr: $ => $._expr,

    _expr: $ =>
      choice(
        $.case_expr,
        $.cond_expr,
        $.op_expr,
        $.fun_expr,
      ),

    case_expr: $ =>
      choice(
        seq(
          'case',
          field("subject", $._expr),
          'of',
          optional('|'),
          sepBy1('|', field("case", $.case_clause_expr)),
          'end'
        ),
        seq(
          'case',
          field("subject", $._expr),
          'of',
          '[',
          optional('|'),
          sepBy1('|', field("case", $.case_clause_expr)),
          ']'
        ),
      ),

    case_clause_expr: $ =>
      seq(
        field("pattern", $.pattern),
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

    op_expr: $ =>
      choice(
        field("the", $._core_expr),
        prec.left (0, seq(field("arg1", $.op_expr),    field("op", 'or'),         field("arg2", $.op_expr))),
        prec.left (1, seq(field("arg1", $.op_expr),    field("op", 'and'),        field("arg2", $.op_expr))),
        prec.right(2, seq(field("arg1", $._core_expr), field("op", 'contains'),   field("arg2", $.op_expr))),
        prec.left (3, seq(field("arg1", $.op_expr),    field("op", $.comparison), field("arg2", $.op_expr))),
        prec.right(4, seq(field("arg1", $.op_expr),    field("op", '^'),          field("arg2", $.op_expr))),
        prec.right(5, seq(field("arg1", $.op_expr),    field("op", '#'),          field("arg2", $.op_expr))),
        prec.left (6, seq(field("arg1", $.op_expr),    field("op", $.adder),      field("arg2", $.op_expr))),
        prec.left (7, seq(field("arg1", $.op_expr),    field("op", $.multiplier), field("arg2", $.op_expr))),
        prec.right(8, seq(field("negate", $.negate), field("arg", $._core_expr))),
      ),

    comparison: $ => choice('<', '<=', '>', '>=', '=', '=/='),
    adder:      $ => choice('-', '+'),
    multiplier: $ => choice('/', '*', 'mod'),
    negate:     $ => choice('-', 'not'),

    _core_expr: $ =>
      choice(
        $.Int,
        $.Nat,
        $.Tez,
        $.Name,
        $.module_field,
        $.String,
        $.Bytes,
        $.False,
        $.True,
        $.Unit,
        $.annot_expr,
        $.tuple_expr,
        $.list_expr,
        $.None,
        $._fun_call_or_par_or_projection,
        $._map_expr,
        $.set_expr,
        $.record_expr,
        $.update_record,
        $.constr_call,
        $.Some_call,
      ),

    constr_call: $ =>
      seq(
        field("constr", $.constr),
        optional(
          field("arguments", $.arguments)
        ),
      ),

    Some_call: $ =>
      seq(
        field("constr", 'Some'),
        field("arguments", $.arguments),
      ),

    _fun_call_or_par_or_projection: $ =>
      choice(
        $.par_call,
        $.projection_call,
        $.fun_call,
      ),

    par_call: $ =>
      prec.right(1, seq(
        par(field("f", $._expr)),
        optional(field("arguments", $.arguments))
      )),

    projection_call: $ => seq(
      field("f", $._projection),
      optional(field("arguments", $.arguments)),
    ),

    annot_expr: $ =>
      par(seq(
        field("subject", $.op_expr),
        ':',
        field("type", $._type_expr)
      )),

    set_expr: $ => injection('set', $._expr),

    _map_expr: $ =>
      choice(
        $.map_lookup,
        $.map_injection,
        $.big_map_injection,
      ),

    map_injection: $ => injection('map', $.binding),
    big_map_injection: $ => injection('big_map', $.binding),

    map_lookup: $ =>
      seq(
        field("container", $.path),
        brackets(field("index", $._expr)),
      ),

    path: $ => choice($.Name, $._projection),

    module_field: $ =>
      seq(
        field("module", $.Name_Capital),
        '.',
        field("method", $._module_fun),
      ),

    _module_fun: $ =>
      choice(
        $.Name,
        $.map,
        $.or,
        $.and,
        $.remove,
      ),

    or: $ => 'or',
    and: $ => 'and',
    remove: $ => 'remove',

    _projection: $ =>
      choice(
        $.data_projection,
        $.module_projection,
      ),

    data_projection: $ => seq(
      field("struct", $.Name),
      '.',
      sepBy1('.', field("index", $.selection)),
    ),

    module_projection: $ =>
      seq(
        field("module", $.Name_Capital),
        '.',
        field("index", $.Name),
        '.',
        sepBy1('.', field("index", $.selection)),
      ),

    selection: $ => choice($.Name, $.Int),

    record_expr: $ =>
      choice(
        seq(
          'record',
          sepBy(';', field("assignment", $.field_assignment)),
          'end',
        ),
        seq(
          'record',
          '[',
          sepBy(';', field("assignment", $.field_assignment)),
          ']',
        ),
      ),

    update_record: $ =>
      seq(
        field("record", $.path),
        'with',
        ne_injection('record', field("assignment", $.field_path_assignment)),
      ),

    field_assignment: $ =>
      seq(
        field("name", $.Name),
        '=',
        field("_rhs", $._expr),
      ),

    field_path_assignment: $ =>
      seq(
        sepBy1('.', field("index", $.Name)),
        '=',
        field("_rhs", $._expr),
      ),

    fun_call: $ =>
      seq(
        field("f", choice($.Name, $.module_field)),
        field("arguments", $.arguments),
      ),

    tuple_expr: $ => par(sepBy1(',', field("element",  $._expr))),
    arguments:  $ => par(sepBy(',',  field("argument", $._expr))),

    list_expr: $ => choice($._list_injection, 'nil'),

    _list_injection: $ => injection('list', field("element", $._expr)),

    pattern: $ => sepBy1('#', field("arg", $._core_pattern)),

    _core_pattern: $ =>
      choice(
        $.Name,
        '_',
        $.Int,
        $.Nat,
        $.String,
        $.list_pattern,
        $.tuple_pattern,
        $._constr_pattern,
      ),

    list_pattern: $ =>
      choice(
        injection("list", field("element", $._core_pattern)),
        'nil',
        par($.cons_pattern),
      ),

    cons_pattern: $ =>
      seq(
        field("head", $._core_pattern),
        '#',
        field("tail", $.pattern),
      ),

    tuple_pattern: $ =>
      par(sepBy1(',', field("element", $._core_pattern))),

    _constr_pattern: $ => choice(
      $.Unit,
      $.False,
      $.True,
      $.None,
      $.Some_pattern,
      $.user_constr_pattern,
    ),

    Some_pattern: $ =>
      seq(
        field("constr", 'Some'),
        par(field("arg", $._core_pattern)),
      ),

    user_constr_pattern: $ =>
      seq(
        field("constr", $.constr),
        optional(field("arguments", $.tuple_pattern)),
      ),

    ///////////////////////////////////////////

    comment: $ => /\/\/[^\n]*\n/,

    ocaml_comment: $ =>
      seq(
        '(*',
        repeat(choice(
          $.ocaml_comment,
          /'([^'\\]|\\[\\"'ntbr ]|\\[0-9][0-9][0-9]|\\x[0-9A-Fa-f][0-9A-Fa-f]|\\o[0-3][0-7][0-7])'/,
          /"([^\\"]|\\(.|\n))*"/,
          /[A-Za-z_][a-zA-Z0-9_']*/,
          /[^('"{*A-Za-z_]+/,
          '(', "'", '*',
        )),
        '*)'
      ),

    include: $ => seq('#include', $.String),

    String:       $ => /\"(\\.|[^"])*\"/,
    Int:          $ => /-?([1-9][0-9_]*|0)/,
    Nat:          $ => /([1-9][0-9_]*|0)n/,
    Tez:          $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes:        $ => /0x[0-9a-fA-F]+/,
    Name:         $ => /[a-z][a-zA-Z0-9_]*/,
    Name_Capital: $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword:      $ => /[A-Za-z][a-z]*/,

    False:         $ => 'False',
    True:          $ => 'True',
    Unit:          $ => 'Unit',
    None:          $ => 'None',
    skip:          $ => 'skip',
    recursive: $ => 'recursive',
  }
});
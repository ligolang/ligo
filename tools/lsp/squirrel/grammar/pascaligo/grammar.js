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
let left_op = (name, left, right) => op(name, left, right, left)

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

  word: $ => $.Keyword,
  extras: $ => [$.ocaml_comment, $.comment, /\s/],

  rules: {
    Start: $ => sepBy(optional(';'), field("declaration", $._declaration)),

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
        field("typeName", $.TypeName),
        "is",
        field("typeValue", $._type_expr),
      ),

    _type_expr: $ =>
      choice(
        $._fun_type,
        $.sum_type,
        $.record_type,
      ),

    _fun_type: $ =>
      choice(
        $.fun_type,
        $.cartesian
      ),

    fun_type: $ =>
      seq(
        field("domain", $.cartesian),
        '->',
        field("codomain", $._fun_type),
      ),

    cartesian: $ =>
      sepBy1('*',
        choice(
          field("element", $._core_type),
          par(field("element", $._type_expr)),
        ),
      ),

    _core_type: $ =>
      choice(
        $.TypeName,
        $.invokeBinary,
        $.invokeUnary,
        $.michelsonTypeOr,
        $.michelsonTypeAnd,
      ),

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

    invokeBinary: $ =>
      seq(
        field("typeConstr", choice('map', 'big_map')),
        field("arguments", $.type_tuple),
      ),

    invokeUnary: $ =>
      seq(
        field("typeConstr", choice('list', 'set', 'option', 'contract')),
        par(field("arguments", $._type_expr)),
      ),

    map: $ => 'map',
    big_map: $ => 'big_map',
    list: $ => 'list',
    set: $ => 'set',

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
          field("arguments", $._fun_type)
        ),
      ),

    constr: $ => $.Name_Capital,

    record_type: $ =>
      choice(
        seq('record', sepBy(';', field("field", $.field_decl)), 'end'),
        seq('record', '[', sepBy(';', field("field", $.field_decl)), ']'),
      ),

    field_decl: $ =>
      seq(
        field("fieldName", $.FieldName),
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
          field("body", $._let_expr),
        ),
      ),

    _let_expr: $ =>
      choice(
        $.let_expr,
        $._expr,
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
        field("name", $.Name),
        ':',
        field("type", $._param_type),
      ),

    _access: $ => choice('var', 'const'),

    _param_type: $ => $._fun_type,

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
        $._loop,
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
        ne_injection('set', field("key", $._expr)),
      ),

    map_patch: $ =>
      seq(
        'patch',
        field("container", $._path),
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
        field("container", $._path),
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
      seq('{', sepBy1(';', field("statement", $._statement)), '}'),

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

    interactive_expr: $ => $._expr,

    _expr: $ =>
      choice(
        $.case_expr,
        $.cond_expr,
        $._op_expr,
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
        $.module_field,
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
        $.Some_call,
      ),

    _constr_use: $ =>
      choice(
        $.constr_call,
        $.constr
      ),

    constr_call: $ =>
      seq(
        field("constr", $.constr),
        field("arguments", $.arguments)
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
        $._projection,
      ),

    par_call: $ =>
      prec.right(1, seq(
        par(field("f", $._expr)),
        field("arguments", $.arguments),
      )),

    projection_call: $ => prec(1, seq(
      field("f", $._projection),
      field("arguments", $.arguments),
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

    _path: $ => choice($.Name, $._projection),

    _fpath: $ => choice($.FieldName, $._projection),

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
      sepBy1('.', field("index", $._selection)),
    ),

    module_projection: $ =>
      seq(
        field("module", $.Name_Capital),
        '.',
        field("index", $.Name),
        '.',
        sepBy1('.', field("index", $._selection)),
      ),

    _selection: $ => choice($.FieldName, $.Int),

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
        field("record", $._path),
        'with',
        ne_injection('record', field("assignment", $.field_path_assignment)),
      ),

    field_assignment: $ =>
      seq(
        field("name", $.FieldName),
        '=',
        field("_rhs", $._expr),
      ),

    field_path_assignment: $ =>
      seq(
        field("lhs", $._fpath),
        '=',
        field("_rhs", $._expr),
      ),

    fun_call: $ =>
      seq(
        field("f", choice($.Name, $.module_field)),
        field("arguments", $.arguments),
      ),

    tuple_expr: $ => par(sepBy1(',', field("element", $._expr))),
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
      ),

    var_pattern: $ => field("name", $.Name),

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
      $.Some_pattern,
      $.user_constr_pattern,
    ),

    Some_pattern: $ =>
      seq(
        field("constr", 'Some'),
        par(field("arg", $._pattern)),
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

    include: $ => seq('#include', field("filename", $.String)),

    String: $ => choice(/\"(\\.|[^"])*\"/, /{\|(\\.|[^\|])*\|}/),
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,
    FieldName: $ => /[a-z][a-zA-Z0-9_]*/,
    TypeName: $ => /[a-z][a-zA-Z0-9_]*/,
    Name: $ => /[a-z][a-zA-Z0-9_]*/,
    Name_Capital: $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword: $ => /[A-Za-z][a-z]*/,

    False: $ => 'False',
    True: $ => 'True',
    Unit: $ => 'Unit',
    None: $ => 'None',
    skip: $ => 'skip',
    recursive: $ => 'recursive',
  }
});

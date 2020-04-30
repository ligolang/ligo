let sepBy1 = (sep, rule) =>
  seq(
    rule,
    repeat(seq(sep, rule)),
    optional(sep)
  )

let sepBy = (sep, rule) => optional(sepBy1(sep, rule))

let par      = x => seq('(', x, ')')
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

let tuple  = x => seq(x, ',', x, repeat(seq(',', x)))
let list__ = x => brackets(sepBy(';', x))

let nseq = x => seq(x, repeat(x))

let op = (l, x, r, def) =>
  choice(
    seq(l, x, r),
    def,
  )

let rop = (l, x, r) => op(l, x, r, r)
let lop = (l, x, r) => op(l, x, r, l)

module.exports = grammar({
  name: 'CAMLigo',

  word:   $ => $.Keyword,
  extras: $ => [$.ocaml_comment, $.comment, /\s/],

  conflicts: $ => [[$.call, $.if_then_else]],

  rules: {
    // debug: $ => $.expr,

    contract: $ => repeat($.declaration),

    expr: $ =>
      choice(
        $.let_expr,
        $.op_call,
      ),

    let_expr: $ =>
      prec.left(0,
        seq(
          'let',
          $.let_binding,
          repeat($.attr),
          'in',
          $.expr,
        ),
      ),

    op_call: $ =>
      $.tuple,

    tuple: $ => prec.left(2, rop($.tuple, ',', $.disj)),
    disj: $ => prec.left(3, rop($.disj, $.or, $.conj)),
    conj: $ => prec.left(4, rop($.conj, $.and, $.comp)),
    comp: $ => prec.left(5, rop($.comp, $.compare, $.cat)),
    cat:  $ => prec.left(6, lop($.cons, $.catenate, $.cat)),
    cons: $ => prec.right(7, lop($.add, $.consolidate, $.cons)),
    add:  $ => prec.left(8, rop($.add, $.plus, $.mult)),
    mult: $ => prec.left(9, rop($.mult, $.times, $.unary)),

    unary: $ =>
      seq(
        optional($.negate),
        $.call,
      ),

    or: $ => choice('||', 'or'),
    and: $ => '&&',
    compare: $ => choice('<', '<=', '>', '>=', '=', '<>'),
    catenate: $ => '^',
    consolidate: $ => '::',
    plus: $ => choice('+', '-'),
    times: $ => choice('*', '/', 'mod'),
    negate: $ => choice('-', 'not'),

    call: $ =>
      prec.left(1,
        choice(
          seq($.call, $.term),
          $.term,
        ),
      ),

    term: $ =>
      choice(
        $.ref,
        $.if_then_else,
        $.literal,
        $.match,
      ),

    match: $ =>
      prec.right(0,
        seq(
          'match',
          $.expr,
          'with',
          optional('|'),
          sepBy1('|', $.alt),
        ),
      ),

    alt: $ =>
      prec.right(0,
        seq(
          $.pattern,
          '->',
          $.expr,
        ),
      ),

    literal: $ =>
      choice(
        $.Nat,
        $.Int,
        $.Tez,
        $.String,
        $.Bytes,
        $.group,
        $.annotation,
        $.list,
        $.lambda,
        $.unit,
        $.record,
        $.record_update,
      ),

    record: $ =>
      seq(
        '{',
        sepBy1(';', $.record_field),
        '}',
      ),

    record_update: $ =>
      seq(
        '{',
        $.name,
        'with',
        sepBy1(';', $.record_field_update),
        '}',
      ),

    record_field_update: $ =>
      seq(
        sepBy1('.', $.name),
        '=',
        $.expr,
      ),

    record_field: $ =>
      seq(
        $.name,
        '=',
        $.expr,
      ),

    unit: $ => seq('(', ')'),

    lambda: $ =>
      seq(
        'fun',
        repeat($.pattern_term),
        '->',
        $.expr,
      ),

    list: $ =>
      seq(
        '[',
        sepBy(';', $.expr),
        ']',
      ),

    annotation: $ =>
      seq(
        '(',
        $.expr,
        ':',
        $.type_expr,
        ')',
      ),

    group: $ =>
      seq(
        '(',
        $.expr,
        ')',
      ),

    if_then_else: $ =>
      prec.right(0,
        seq(
          'if',
          $.expr,
          'then',
          $.expr,
          'else',
          $.expr,
        ),
      ),

    ref: $ =>
      choice(
        $.module_qualified,
        $.struct_qualified,
        $.name,
        $.constr,
      ),

    module_qualified: $ =>
      seq(
        $.constr,
        '.',
        $.name,
      ),

    struct_qualified: $ =>
      seq(
        $.name,
        nseq(seq('.', $.accessor)),
      ),

    accessor: $ => choice($.name, $.Int),

    declaration: $ =>
      choice(
        $.type_decl,
        $.let_declaration,
      ),

    type_decl: $ =>
      seq(
        'type',
        $.type_name,
        '=',
        $.type_expr,
      ),

    type_expr: $ =>
      choice(
        $.fun_type,
        $.sum_type,
        $.record_type,
      ),

    fun_type: $ =>
      seq(
        $.cartesian,
        optional(seq('->', $.fun_type)),
      ),

    cartesian: $ =>
      prec.left(1,
        seq(
          $.core_type,
          optional(seq('*', sepBy('*', $.core_type))),
        ),
      ),

    core_type: $ =>
      choice(
        $.type_name,
        par($.type_expr),
        $.module_type_name,
        $.type_application
      ),

    module_type_name: $ =>
      seq(
        $.module_name,
        '.',
        $.type_name,
      ),

    type_application: $ =>
      seq(
        choice($.core_type, $.type_tuple),
        $.type_name,
      ),

    type_tuple: $ =>
      par(tuple($.type_expr)),

    sum_type: $ =>
      seq(
        optional('|'),
        sepBy1('|', $.variant),
      ),

    variant: $ =>
      seq(
        $.constr,
        optional(seq('of', $.fun_type)),
      ),

    record_type: $ =>
      seq(
        '{',
        sepBy(';', $.field_decl),
        '}',
      ),

    field_decl: $ =>
      seq(
       $.field_name,
       ':',
       $.type_expr,
      ),

    let_declaration: $ =>
      seq(
        'let',
        optional($.rec),
        $.let_binding,
        repeat($.attr),
      ),

    let_binding: $ =>
      choice(
        seq(
          $.name,
          nseq($.pattern_term),
          optional($.type_annotation),
          '=',
          $.expr,
        ),
        seq(
          $.pattern,
          optional($.type_annotation),
          '=',
          $.expr,
        ),
      ),

    type_annotation: $ => seq(':', $.type_expr),

    pattern_term: $ =>
      choice(
        $.pattern_record,
        $.constr,
        $.pattern_list,
        $.pattern_constant,
        $.name,
        $.pattern_grouped,
      ),

    pattern_grouped: $ =>
      seq(
        '(',
        $.pattern,
        optional($.type_annotation),
        ')',
      ),

    pattern_list: $ => list__($.pattern),
    pattern_constant: $ =>
      choice(
        $.Nat,
        $.String,
        $.Bytes,
        $.Int,
      ),

    pattern_record: $ =>
      seq(
        '{',
        sepBy1(';', $.pattern_record_field),
        '}',
      ),

    pattern_record_field: $ =>
      seq(
        $.name,
        '=',
        $.pattern,
      ),

    pattern:       $ => sepBy1('::', $.pattern_tuple),
    pattern_tuple: $ => sepBy1(',', $.construct),

    construct: $ =>
      choice(
        seq($.constr, nseq($.pattern_term)),
        $.pattern_term,
      ),

 ///////////////////////////////////////////

    type_name: $ => $.ident,
    field_name: $ => $.ident,
    fun_name: $ => $.ident,
    struct_name: $ => $.ident,
    var: $ => $.ident,
    module_name: $ => $.Name_Capital,
    constr: $ => $.Name_Capital,
    ident: $ => $.Name,
    name: $ => $.Name,

    comment: $ => /\/\/[^\n]*\n/,

    ocaml_comment: $ => seq(
      '(*',
      repeat(choice(
        $.ocaml_comment,
        /[^\(]|\(?!\*/,
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

    False:        $ => 'False',
    True:         $ => 'True',
    Unit:         $ => 'Unit',
    None:         $ => 'None',
    Some:         $ => 'Some',
    skip:         $ => 'skip',
    rec:          $ => 'rec',

    attr:         $ => seq('[@@', $.name, ']'),
  }
});
const PREC = {

  CALL: 300000000,
  OR: 0,
  AND: 1,
  COMPARE: 3,
  CONCAT: 5,
  PLUS: 6,
  MINUS: 6,
  MUL: 7,
  DIV: 7,

  ATTR: 1,
  TYPE: 101,
  LET: 100,
  EXPR: 100000000,
  CONSTR: 100000,
  LAM: 1000,
};

const OPS = [
  ['+', PREC.PLUS],
  ['-', PREC.MINUS],
  ['mod', PREC.DIV],
  ['/', PREC.DIV],
  ['*', PREC.MUL],
  ['++', PREC.CONCAT],
  ['<', PREC.COMPARE],
  ['>', PREC.COMPARE],
  ['<=', PREC.COMPARE],
  ['>=', PREC.COMPARE],
  ['==', PREC.COMPARE],
  ['!=', PREC.COMPARE],
  ['&&', PREC.COMPARE],
  ['||', PREC.OR],
]

let sepBy1 = (sep, rule) =>
  seq(
    rule,
    repeat(seq(sep, rule)),
    optional(sep)
  )

let sepBy = (sep, rule) => optional(sepBy1(sep, rule))

let par = x => seq('(', x, ')')
let opar = x => seq(optional('('), x, optional(')'))
let brackets = x => seq('[', x, ']')
let block = x => seq('{', x, '}')

let tuple = x => seq(x, ',', x, repeat(seq(',', x)))
let list__ = x => brackets(sepBy(';', x))

let nseq = x => seq(x, repeat(x))

module.exports = grammar({
  name: 'ReasonLigo',

  word: $ => $.Keyword,
  extras: $ => [$.oneline_comment, $.block_comment, /\s/],

  conflicts: $ => [
    [$._fun_type, $.type_application],
    [$._pattern, $.tuple_pattern],
    [$._expr], // TODO: left-precendence is used for tuple_expr, right for nested `let`s
    [$._exprf],

    [$.tuple_expr, $.parameters],
    [$.record_field, $._accessor], // TODO
    [$.variant], // TODO: some alien stuff
    [$.sum_type], // TODO: fix in `let_declaration`.
    [$.bracket_block], // TODO: same as $.lambda_body
    [$.type_decl],
    [$.lambda],
    [$._instruction, $._expr], // switch_instr
    [$._functional_block, $.bracket_block], // TODO: remove
    [$.list_access], // TODO: remove
    [$.list_access, $._ident], // TODO
    [$.record_field, $._ident], // TODO
    [$._core_pattern, $._expr], //TODO: remove as soon as possible
    [$.lambda_call], // TODO: remove
    [$.lambda_call, $.binary_call],
    [$._expr, $.bracket_block], // TODO
    [$.lambda_call, $.parameters], // TODO
    [$["-"], $.negate], // unary vs binary calls

    [$.parameters, $.annot_expr], // TODO
    [$.type_tuple, $._core_type],

    [$._pattern, $.annot_pattern], // TODO: constr (pat) case
    [$.annot_expr, $._type_annotation],
    [$.annot_expr, $.lambda_call],
    [$.annot_expr, $._functional_block],

    // TODO: those are produced in conflicts of `fun_call` and `expr` in bracket blocks
    [$.annot_expr, $._exprf],
    [$.lambda_call, $._exprf],
    [$.bracket_block, $._exprf],
    [$._functional_block, $._exprf],
    [$.tuple_expr, $._exprf],
    [$._annot_fun_call, $._exprf],
  ],

  rules: {
    contract: $ => sepBy(optional(';'), field("declaration", $._declaration)),

    _expr: $ =>
      prec(PREC.EXPR,
        seq(
          choice(
            $.lambda_call,
            $.tuple_expr,
            $._literal,
            // $.fun_call,
            $.constructor_call,
            $.unary_call,
            $.binary_call,
            $.annot_expr,
            $.list_expr,
            $.record_expr,
            $._ref,
            $._ident,
            $.switch_instr,
          ),
        ),
      ),

    // TODO: workaround to move `fun_call` out of `expr` needed in bracket blocks
    _exprf: $ => choice($.fun_call, $._expr),

    _annot_expr: $ =>
      par($.annot_expr),

    annot_expr: $ =>
      seq(
        field("subject", $._expr),
        ':',
        field("type", $._type_expr),
      ),

    // TODO: same as tuple_pattern
    // TODO: to annotate expression we do not need to enclose it in brackets
    // TODO: possible ((a : p) : q, b)
    tuple_expr: $ =>
      par(sepBy1(',',
        seq(
          field("element", choice(
            $.annot_expr,
            $.fun_call,
            $._expr,
          )),
        )
      )),

    constructor_call: $ =>
      prec(PREC.CONSTR,
        seq(
          field("constructor",
            choice(
              $._constr,
              $.module_qualified,
            ),
          ),
          field("parameters", $.parameters),
        ),
      ),

    lambda_call: $ =>
      seq(
        par(field("lambda", $.lambda)),
        repeat1(
          choice(
            field("arguments", $._expr),
            par(field("arguments", $.lambda))
          )
        ), // TODO: workaround to move lambda out of expr
      ),

    fun_call: $ =>
      prec.right(PREC.CALL,
        seq(
          field("f", $.Name), // TODO: may be qualified
          field("arguments", $.parameters),
        )
      ),

    binary_call: $ => choice(
      ...OPS
        .map(([op, precendence]) =>
          prec.left(precendence, seq(
            field("left", $._exprf),
            field("op", $[op]),
            field("right", $._exprf),
          ))
        )
    ),

    // Workaround to make operators to be instructions as well
    // so that they won't be stripped when naming them as fields
    ...OPS.reduce(
      (acc, [e, _]) => ({ ...acc, [e]: $ => seq(e) }), {}
    ),

    unary_call: $ => prec.right(8,
      seq(
        field("negate", $.negate),
        field("arg", $._exprf)
      )
    ),

    negate: $ => choice('-', '!'),

    // TODO: refactor
    call: $ =>
      prec.left(1,
        choice(
          seq($.call, par($._expr)),
          par($._expr),
        ),
      ),

    switch_instr: $ =>
      prec.left(0, // TODO
        seq(
          'switch',
          choice(
            field('subject', $.Name),
            par(field('subject', $._exprf)),
          ),
          block(seq(
            optional('|'),
            sepBy1('|', field('case', $.alt)),
          )),
        ),
      ),

    // TODO: it may parse expr with ';' attached to the end
    alt: $ =>
      seq(
        field("pattern", $._pattern),
        '=>',
        field("body", $._functional_block),
        optional(';'),
      ),


    _literal: $ =>
      choice(
        $.Nat,
        $.Int,
        $.Bool,
        $.Tez,
        $.String,
        $.Bytes,
        $.Unit,
        $.Nil,
        $.wildcard,
      ),

    record_expr: $ =>
      seq(
        block(seq(
          sepBy1(',', field("assignment", choice(
            $.spread,
            $.record_field,
          ))),
          optional($._ident), // TODO: not need to be here
        ))
      ),

    spread: $ => seq(
      '...',
      field("name", $._ident), // TODO: may be possible expression
    ),

    record_field: $ =>
      seq(
        field("name", sepBy1('.', $.Name)),
        ':',
        field("value", $._expr),
      ),

    _statement: $ => choice(
      $._instruction,
    ),

    _instruction: $ =>
      prec(PREC.EXPR, choice(
        $.conditional,
        $.switch_instr,
        $.let_declaration,
      )),

    lambda: $ =>
      seq(
        field("lambda_head", seq(
          field("arguments", $.parameters),
          optional(seq(":",
            field("lambda_type",
              choice($._core_type, $.type_tuple),
            ),
          ))
        )),
        '=>',
        field("lambda_body", $._functional_block)
      ),

    parameters: $ =>
      par(
        sepBy(',', field("parameter", seq(
          choice($._exprf, $.lambda), // TODO: workaround to move lambda our of expr
          optional($._type_annotation), // TODO: move it somewhere
        )))
      ),

    _pattern: $ =>
      choice(
        $.cons_pattern,
        $.annot_pattern,
        $._core_pattern,
      ),

    cons_pattern: $ =>
      brackets(
        seq(
          field("head", $._core_pattern),
          ',',
          field("tail", choice(
            $.spread,
            $._pattern,
          )),
        )
      ),

    annot_pattern: $ =>
      par(
        seq(
          field("subject", $._core_pattern),
          ':',
          field("type", $._type_expr),
        )
      ),

    _core_pattern: $ =>
      prec.right(PREC.CONSTR,
        seq(
          choice(
            $.constr_pattern,
            $.tuple_pattern,
            $._literal,
            $.Name,
          ),
        )
      ),

    tuple_pattern: $ =>
      par(tuple(field("element", $._pattern))),

    constr_pattern: $ =>
      prec.right(PREC.CONSTR, // TODO: left precendence fails
        seq(
          field("constr", $._constr),
          optional(
            opar(
              field("arguments", $._pattern)
            ),
          )),
      ),

    list_expr: $ => brackets(
      sepBy(',', field("element", choice(
        $.spread,
        $._expr,
      )))
    ),


    conditional: $ =>
      prec.left(PREC.LET,
        seq(
          'if',
          par(field("selector", $._expr)),
          field("then", $.bracket_block),
          optional(seq(
            'else',
            field("else", $.bracket_block),
          )),
        ),
      ),

    _ref: $ =>
      choice(
        $.module_qualified,
        $.struct_qualified,
        $._constr,
        $.list_access,
      ),

    module_qualified: $ =>
      seq(
        field("module", $._constr),
        nseq(seq('.', field("method", $.Name))), // TODO: capital letters
      ),

    // TODO: may be the same as `list_access`
    struct_qualified: $ =>
      seq(
        field("struct", $.Name),
        nseq(seq('.', field("method", $._accessor))),
      ),

    _accessor: $ => choice($.Name, $.Int),

    list_access: $ =>
      // prec.left(8,
      seq(
        field("name", $.Name),
        repeat1(brackets(field("indexes", $.Int))),
        // ),
      ),

    _declaration: $ =>
      field("declaration",
        choice(
          $.type_decl,
          $.let_declaration,
          $.include,
          $.attr_decl,
        )
      ),

    attr_decl: $ =>
      prec.left(PREC.ATTR,
        nseq(brackets(
          seq('@', field("name", $.Name))
        ))
      ),

    type_decl: $ =>
      seq(
        'type',
        field("type_name", $.TypeName),
        '=',
        field("type_value", $._type_expr),
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
        $._core_type,
        $.type_tuple,
      ),

    fun_type: $ =>
      seq(
        field("domain", choice($._core_type, $.type_tuple)),
        '=>',
        field("codomain", $._fun_type),
      ),

    _core_type: $ =>
      choice(
        $.type_application,
        $.TypeName,
        $.type_string,
        $.module_TypeName,
      ),

    module_TypeName: $ =>
      seq(
        $.module_name,
        '.',
        $.TypeName,
      ),

    type_application: $ =>
      seq(
        field("functor", $._core_type),
        par(sepBy(',', field("parameter", $._type_expr))),
      ),

    type_string: $ => $.String,

    type_tuple: $ =>
      par(sepBy1(',', field("element", $._type_expr))),

    sum_type: $ =>
      seq(
        optional('|'),
        sepBy1('|', field("variant", $.variant)),
      ),

    variant: $ =>
      seq(
        field("constructor", $._constr),
        optional(par(field("arguments", $._fun_type))),
      ),

    record_type: $ =>
      seq(
        '{',
        sepBy(',', field("field", $.field_decl)),
        '}',
      ),

    field_decl: $ =>
      seq(
        field("field_name", $._field_name),
        ':',
        field("field_type", $._type_expr),
      ),

    te: $ => choice(
      $.lambda,
      $._expr,
    ),

    _functional_block: $ =>
      choice(
        $._statement,
        $._expr,
        $.fun_call,
        $.bracket_block,
      ),

    _annot_fun_call: $ => // Workaround used only in bracket blocks
      par(seq(
        $.fun_call,
        ':',
        $._type_expr,
      )),

    bracket_block: $ =>
      prec.right(1000000, // TODO: precendence
        block(
          choice(
            seq(
              sepBy(';', field("statement", choice(
                $.constructor_call,
                seq(
                  optional($.attr_decl),
                  $._statement,
                ),
                $.fun_call,
                // $._expr, // TODO: use only `fun_call` from there, somewhat fails if called directly
              ))),
              optional(seq(
                opar(
                  field("return",
                    choice(
                      $._expr,
                      seq( // TODO: maybe not all returned function calls need to be annotated
                        $.fun_call,
                        ':',
                        $._type_expr, // TODO: maybe we'll need to annotate fields here, but so far we just return multiple 'return's
                      )
                    )
                  )
                ),
                optional(';'),
              ))),
            $.bracket_block,
          ), // TODO conflict: _group bracket_block
        )
      ),

    let_declaration: $ =>
      prec.left(PREC.LET,
        seq(
          'let',
          optional($.rec),
          field("binding", choice(
            sepBy1(',', $._core_pattern),
          )),
          optional(seq(':', field("type", $._type_expr))),
          '=',
          field("let_value",
            choice(
              $.lambda,
              $._functional_block,
            ),
          ),
        )
      ),

    _type_annotation: $ =>
      seq(
        ':',
        field("type", $._type_expr)
      ),

    pattern_term: $ =>
      choice(
        $.Name,
        $.pattern_grouped,
      ),

    pattern_grouped: $ =>
      seq(
        '(',
        $._core_pattern,
        optional($._type_annotation),
        ')',
      ),

    pattern_list: $ => list__($._pattern),
    pattern_constant: $ =>
      choice(
        $.Nat,
        $.String,
        $.Bytes,
        $.Int,
      ),

    pattern_tuple: $ => sepBy1(',', $._construct),

    _construct: $ =>
      choice(
        seq($._constr, nseq($.pattern_term)),
        par($.pattern_term),
      ),

    ///////////////////////////////////////////

    _field_name: $ => $.Name,
    fun_name: $ => $.Name,
    struct_name: $ => $.Name,
    var: $ => $.Name,
    module_name: $ => $.Name_Capital,
    _constr: $ => $.Name_Capital,
    // TODO: remove
    _ident: $ => $.Name,

    comment: $ => choice(
      $.oneline_comment,
      $.block_comment
    ),

    oneline_comment: $ => token(seq('//', /.*/)),

    block_comment: $ => seq(
      '/*',
      repeat(/./),
      '*/'
    ),

    include: $ => seq('#include', $.String),

    String: $ => /\"(\\.|[^"])*\"/,
    Int: $ => /-?([1-9][0-9_]*|0)/,
    Nat: $ => /([1-9][0-9_]*|0)n/,
    Tez: $ => /([1-9][0-9_]*|0)(\.[0-9_]+)?(tz|tez|mutez)/,
    Bytes: $ => /0x[0-9a-fA-F]+/,
    Name: $ => /[a-z][a-zA-Z0-9_]*/,
    TypeName: $ => /[a-z][a-zA-Z0-9_]*/,
    Name_Capital: $ => /[A-Z][a-zA-Z0-9_]*/,
    Keyword: $ => /[A-Za-z][a-z]*/,
    Bool: $ => choice($.False, $.True),

    include: $ => seq('#include', field("filename", $.String)),

    False: $ => 'false',
    True: $ => 'true',
    Unit: $ => '()',
    Nil: $ => '[]',
    None: $ => 'None',
    Some: $ => 'Some',
    skip: $ => 'skip',
    rec: $ => 'rec',
    wildcard: $ => '_',

    attr: $ => seq('[@@', $.Name, ']'),
  }
});
[@@@warning "-30"]

type op =
  | SHARP (* *)
  | CONTAINS (* CONTAINS *)
  | CARET (* ^ *)
  | PLUS (* + *)
  | MINUS (* - *)
  | STAR (* * *)
  | SLASH (* / *)
  | WORD_MOD (* mod *)
  | PRCENT (* % *)
  | WORD_OR
  | WORD_AND
  | WORD_NOT
  | DPIPE (* || *)
  | DAMPERSAND (* && *)
  | LT (* < *)
  | GT (* > *)
  | GE (* <= *)
  | LE (* >=*)
  | SEQ (* = *)
  | DEQ (* == *)
  | LTGT (* <> *)
  | EQ_SLASH_EQ (* =/= *)
  | WORD_LSL
  | WORD_LSR
  | WORD_XOR
  | WORD_LOR
  | WORD_LAND
  | WORD_LXOR
  | EX_MARK (* ! *)
  | DCOLON (* :: *)

and operator = op Simple_utils.Location.wrap
[@@deriving compare, equal, yojson, map, fold, iter, sexp, compare, hash]

type 'expr binary_op =
  { operator : operator
  ; left : 'expr
  ; right : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

type 'expr unary_op =
  { operator : operator
  ; arg : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

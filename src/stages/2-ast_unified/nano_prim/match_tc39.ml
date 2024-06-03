module Ne_list = Simple_utils.Ne_list

type ('expr, 'pattern) t =
  { subject : 'expr
  ; match_clauses : ('expr, 'pattern) match_clauses
  }

and ('expr, 'pattern) match_clauses =
  | AllClauses of ('expr, 'pattern) match_clause Ne_list.t * 'expr option
  | DefaultClause of 'expr

and ('expr, 'pattern) match_clause =
  { filter : 'pattern
  ; clause_expr : 'expr
  }
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

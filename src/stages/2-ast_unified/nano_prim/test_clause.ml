type ('instruction, 'block) t =
  | ClauseInstr of 'instruction
  | ClauseBlock of 'block
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

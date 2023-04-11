type ('lhs, 'rhs) t =
  | Punned of 'lhs Simple_utils.Location.wrap
  | Complete of ('lhs * 'rhs)
[@@deriving yojson, map, iter, fold, sexp, eq, compare, hash]

let is_pun = function
  | Punned _ -> true
  | Complete _ -> false

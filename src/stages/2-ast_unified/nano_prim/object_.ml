type 'expr property =
  | Punned_property of 'expr
  | Property of 'expr * 'expr
  | Property_rest of 'expr

and 'expr t = 'expr property Simple_utils.List.Ne.t
[@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]

type 'ty t = (string list[@eq.ignore] [@hash.ignore] [@compare.ignore]) * 'ty * 'ty
[@@deriving yojson, map, fold, iter, sexp, eq, compare, hash]

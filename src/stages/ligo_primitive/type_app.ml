type ('ty_path, 'ty_exp) t =
  { type_operator : 'ty_path
  ; arguments : 'ty_exp list
  }
[@@deriving eq, compare, yojson, hash, fold, map]

let pp f g ppf ({ type_operator; arguments } : ('a, 'b) t) : unit =
  Format.fprintf
    ppf
    "%a%a"
    f
    type_operator
    Simple_utils.PP_helpers.(list_sep_d_par g)
    arguments

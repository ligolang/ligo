let pp_as_sexp sexper ppf x = Format.fprintf ppf "%a" Sexp.pp_hum (sexper x)
let program = pp_as_sexp S_exp.sexp_of_program
let ty_expr = pp_as_sexp S_exp.sexp_of_ty_expr
let expr = pp_as_sexp S_exp.sexp_of_expr
let statement = pp_as_sexp S_exp.sexp_of_statement
let pattern = pp_as_sexp S_exp.sexp_of_pattern

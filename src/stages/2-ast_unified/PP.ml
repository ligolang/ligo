open Types

let program ppf (p : program) =
  Format.fprintf ppf "%a" Sexp.pp_hum (S_exp.sexp_of_program p)

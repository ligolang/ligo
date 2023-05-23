let dummy_loc : _ Recursion_schemes.Catamorphism.fold =
  let open Simple_utils.Location in
  let rm_loc = fun f x -> f x.wrap_content in
  { expr = rm_loc (Combinators.make_e ~loc:dummy)
  ; ty_expr = rm_loc (Combinators.make_t ~loc:dummy)
  ; pattern = rm_loc (Combinators.make_p ~loc:dummy)
  ; statement  = rm_loc (Combinators.make_s ~loc:dummy) 
  ; block  = rm_loc (Combinators.make_b ~loc:dummy)
  ; mod_expr  = rm_loc (Combinators.make_m ~loc:dummy)
  ; instruction  = rm_loc (Combinators.make_i ~loc:dummy)
  ; declaration = rm_loc (Combinators.make_d ~loc:dummy)
  ; program_entry = Combinators.make_pe
  ; program  = Combinators.make_prg
  }

let pp_as_sexp ~show_loc folder sexper ppf x =
  (* This is a trick to avoid printing locations in sexp unless specified *)
  let x = if show_loc then x else folder ~f:dummy_loc x in
  Format.fprintf ppf "%a" Sexp.pp_hum (sexper x)



let program ?(show_loc = false) = pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_program S_exp.sexp_of_program
let ty_expr ?(show_loc = false) = pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_ty_expr S_exp.sexp_of_ty_expr
let expr ?(show_loc = false) = pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_expr S_exp.sexp_of_expr
let statement ?(show_loc = false) = pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_statement S_exp.sexp_of_statement
let pattern ?(show_loc = false) = pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_pattern S_exp.sexp_of_pattern

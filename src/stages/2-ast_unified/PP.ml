let dummy_loc : _ Recursion_schemes.Catamorphism.fold =
  let open Simple_utils.Location in
  let rm_loc f x = f x.wrap_content in
  { expr = rm_loc (Combinators.make_e ~loc:dummy)
  ; ty_expr = rm_loc (Combinators.make_t ~loc:dummy)
  ; pattern = rm_loc (Combinators.make_p ~loc:dummy)
  ; statement = rm_loc (Combinators.make_s ~loc:dummy)
  ; block = rm_loc (Combinators.make_b ~loc:dummy)
  ; mod_expr = rm_loc (Combinators.make_m ~loc:dummy)
  ; instruction = rm_loc (Combinators.make_i ~loc:dummy)
  ; declaration = rm_loc (Combinators.make_d ~loc:dummy)
  ; program_entry = Combinators.make_pe
  ; program = Combinators.make_prg
  ; sig_entry = Combinators.make_sig_entry
  ; sig_expr = rm_loc (Combinators.make_sig_expr ~loc:dummy)
  }


module Hidden_sexp = S_exp.M (struct
  let ty_expr = true
  let pattern = true
  let instruction = true
  let statement = true
  let block = true
  let declaration = true
  let mod_expr = true
  let expr = true
  let program = true
  let program_entry = true
  let sig_entry = true
  let sig_expr = true
end)

let pp_as_sexp ~show_loc folder sexper ppf x =
  (* This is a trick to avoid printing locations in sexp unless specified *)
  let x = if show_loc then x else folder ~f:dummy_loc x in
  Format.fprintf ppf "%a" Sexp.pp_hum (sexper x)


let program ?(show_loc = false) ~(hide_sort : string list) =
  let module Hidden_sexp =
    S_exp.M (struct
      let ty_expr = List.exists ~f:(String.equal "ty_expr") hide_sort
      let pattern = List.exists ~f:(String.equal "pattern") hide_sort
      let instruction = List.exists ~f:(String.equal "instruction") hide_sort
      let statement = List.exists ~f:(String.equal "statement") hide_sort
      let block = List.exists ~f:(String.equal "block") hide_sort
      let declaration = List.exists ~f:(String.equal "declaration") hide_sort
      let mod_expr = List.exists ~f:(String.equal "mod_expr") hide_sort
      let expr = List.exists ~f:(String.equal "expr") hide_sort
      let program = List.exists ~f:(String.equal "program") hide_sort
      let program_entry = List.exists ~f:(String.equal "program_entry") hide_sort
      let sig_entry = List.exists ~f:(String.equal "sig_expr") hide_sort
      let sig_expr = List.exists ~f:(String.equal "sig_entry") hide_sort
    end)
  in
  pp_as_sexp
    ~show_loc
    Recursion_schemes.Catamorphism.cata_program
    Hidden_sexp.sexp_of_program


let ty_expr ?(show_loc = false) =
  pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_ty_expr S_exp.sexp_of_ty_expr


let expr ?(show_loc = false) =
  pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_expr S_exp.sexp_of_expr


let statement ?(show_loc = false) =
  pp_as_sexp
    ~show_loc
    Recursion_schemes.Catamorphism.cata_statement
    S_exp.sexp_of_statement


let pattern ?(show_loc = false) =
  pp_as_sexp ~show_loc Recursion_schemes.Catamorphism.cata_pattern S_exp.sexp_of_pattern


let declaration ?(show_loc = false) =
  pp_as_sexp
    ~show_loc
    Recursion_schemes.Catamorphism.cata_declaration
    S_exp.sexp_of_declaration

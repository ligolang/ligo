open Typer_new.Compare_renaming

let deduce_and_clean_result : Ast_typed.deduce_and_clean_result cmp  = fun expected actual ->
  let open Ast_typed in
  let { deduced = a1 ; cleaned = a2 } = expected in
  let { deduced = b1 ; cleaned = b2 } = actual in
  c_constructor_simpl_list a1 b1 <? fun () -> c_typeclass_simpl a2 b2

let compare_and_check_vars_deduce_and_clean_result expected actual =
  compare_and_check_vars
    ~compare:deduce_and_clean_result
    ~print_whole:Ast_typed.PP.deduce_and_clean_result
    expected
    actual

open Ast_unified
open Pass_type
module Location = Simple_utils.Location

(* This is a temporary dynamic check that the unification pass do not emit
node that are reserved for the nano passes *)

let compile ~raise ~disable_initial_check =
  ignore raise;
  let check f x =
    if f (Location.unwrap x) then failwith "Unification emit forbidden nodes"
  in
  let check' f x =
    if f (Location.unwrap x)
    then
      failwith
        (Format.asprintf
           "Unification emit forbidden nodes : %a"
           Sexp.pp
           (S_exp.sexp_of_expr { fp = x }))
  in
  if disable_initial_check
  then `Check Iter.defaults
  else
    `Check
      { Iter.defaults with
        expr = check' expr_is_not_initial
      ; ty_expr = check ty_expr_is_not_initial
      ; pattern = check pattern_is_not_initial
      ; statement = check statement_is_not_initial
      ; mod_expr = check mod_expr_is_not_initial
      ; instruction = check instruction_is_not_initial
      ; declaration = check declaration_is_not_initial
      ; program_entry =
          (fun x ->
            if program_entry_is_not_initial x
            then failwith "Unification emit forbidden nodes")
      ; program = (fun _ -> ())
      }


let pass ~raise ~disable_initial_check =
  morph
    ~name:__MODULE__
    ~compile:(compile ~raise ~disable_initial_check)
    ~decompile:`None
    ~reduction_check:Iter.defaults

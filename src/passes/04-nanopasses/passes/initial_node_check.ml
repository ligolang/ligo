open Ast_unified
open Pass_type
module Location = Simple_utils.Location

let name = __MODULE__
(* This is a temporary dynamic check that the unification pass do not emit
node that are reserved for the nano passes *)

include Flag.No_arg ()

let compile ~raise =
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
  Check
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


let decompile ~raise:_ = Nothing
let reduction ~raise:_ = Iter.defaults

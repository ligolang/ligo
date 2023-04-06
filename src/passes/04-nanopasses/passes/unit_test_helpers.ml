open Simple_utils.Trace
open Ast_unified

let raise : (Errors.t, unit) raise = raise_failwith "test"

let try_with f in_prg =
  try_with
    (fun ~raise ~catch:_ ->
      let _v = f ~raise in_prg in
      print_endline "This test should have failed")
    (fun ~catch:_ e ->
      Format.fprintf
        Format.std_formatter
        "Err : %a"
        Errors.(error_ppformat ~display_format:Dev ~no_colour:false)
        e)


let prg_of_string input = input |> Sexp.of_string |> S_exp.program_of_sexp

let expected_failure_fwd (input : string) (pass : raise:_ -> Pass_type.pass) : unit =
  let in_prg = prg_of_string input in
  let f ~raise = (pass ~raise).program.forward in
  try_with f in_prg


let expected_failure_bwd (input : string) (pass : raise:_ -> Pass_type.pass) : unit =
  let in_prg = prg_of_string input in
  let f ~raise = (pass ~raise).program.backward in
  try_with f in_prg


let expected_sucess_fwd (input : string) (pass : Pass_type.pass) : unit =
  let in_prg = prg_of_string input in
  let out_expr = pass.program.forward in_prg in
  Format.printf "%a" Sexp.pp_hum (S_exp.sexp_of_program out_expr)


let expected_sucess_bwd (input : string) (pass : Pass_type.pass) : unit =
  let in_prg = prg_of_string input in
  let out_expr = pass.program.backward in_prg in
  Format.printf "%a" Sexp.pp_hum (S_exp.sexp_of_program out_expr)


let ( |-> ) = expected_sucess_fwd
let ( <-| ) = expected_sucess_bwd
let ( |->! ) = expected_failure_fwd
let ( !<-| ) = expected_failure_bwd

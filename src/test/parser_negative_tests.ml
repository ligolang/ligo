open Test_helpers
open Trace

type 'a sdata = { erroneous_source_file : string ; parser : string -> 'a result }
let pascaligo_sdata = {
  erroneous_source_file = "../passes/1-parser/pascaligo/all.ligo" ;
  parser = Parser.Pascaligo.parse_expression }
let cameligo_sdata = {
  erroneous_source_file = "../passes/1-parser/cameligo/all.mligo" ;
  parser = Parser.Cameligo.parse_expression }
let reasonligo_sdata = {
  erroneous_source_file = "../passes/1-parser/reasonligo/all.religo" ;
  parser = Parser.Reasonligo.parse_expression }

let get_exp_as_string filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

let assert_syntax_error sdata () =
  let%bind _l = bind_iter_list
    (fun entry -> Assert.assert_fail @@ sdata.parser entry)
    (get_exp_as_string sdata.erroneous_source_file) in
  ok ()

let () =
  Printexc.record_backtrace true ;
  run_test @@ test_suite "LIGO" [
    test_suite "Parser negative tests" [
      test "pascaligo"  @@ assert_syntax_error pascaligo_sdata ;
      test "cameligo"   @@ assert_syntax_error cameligo_sdata ;
      test "reasonligo" @@ assert_syntax_error reasonligo_sdata ;
    ]
  ] ;
  ()

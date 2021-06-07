open Test_helpers
open Trace
open Main_errors
open Function

type ('a,'err) sdata = {
  erroneous_source_file : string ;
  preproc : string -> (Buffer.t * (string * string) list, 'err) result;
  parser : Buffer.t -> ('a,'err) result
}

let pascaligo_sdata = {
  erroneous_source_file =
    "../passes/02-parsing/pascaligo/all.ligo" ;
  preproc =
    trace preproc_tracer <@
    Preprocessing.Pascaligo.preprocess_string [] ;
  parser =
    trace parser_tracer <@
      Parsing.Pascaligo.parse_expression
}

let cameligo_sdata = {
  erroneous_source_file =
    "../passes/02-parsing/cameligo/all.mligo";
  preproc =
    trace preproc_tracer <@
    Preprocessing.Cameligo.preprocess_string [];
  parser =
    trace parser_tracer <@
    Parsing.Cameligo.parse_expression
}

let reasonligo_sdata = {
  erroneous_source_file =
    "../passes/02-parsing/reasonligo/all.religo" ;
  preproc =
    trace preproc_tracer <@
    Preprocessing.Reasonligo.preprocess_string [];
  parser =
    trace parser_tracer <@
    Parsing.Reasonligo.parse_expression
}

let get_exp_as_string filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines

let assert_syntax_error sdata () =
  let aux entry =
    Format.printf "Entry : <%s>%!\n" entry ;
    let* c_unit,_ = sdata.preproc entry in
    let result   = sdata.parser c_unit in
    Format.printf "Parsed%!\n" ;
    Assert.assert_fail (test_internal __LOC__) result
  in
  let exps = get_exp_as_string sdata.erroneous_source_file in
  bind_iter_list aux exps


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

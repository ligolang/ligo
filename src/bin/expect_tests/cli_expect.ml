(* exit <> 0 but expected exit = 0 *)
exception Should_exit_good

(* exit = 0 but expected exit <> 0 *)
exception Should_exit_bad

(* ugh, can we avoid this? *)
let () = Ligo_unix.putenv "TERM" "dumb"
let bad_test basename = "../../test/contracts/negative/" ^ basename
let test basename = "../../test/contracts/" ^ basename

(* Temporary breaking *)
let run_ligo args =
  Ligo_prim.Value_var.reset_counter ();
  Ligo_prim.Type_var.reset_counter ();
  Ligo_prim.Module_var.reset_counter ();
  Self_ast_aggregated.reset_counter ();
  Cli.reset_return ();
  let argv = "ligo" :: args in
  let result = Cli.run ~argv () in
  result


let run_ligo_good args =
  let () =
    (* std_lib and generated variables (gen#42) makes it very annoying as an expect test *)
    match args with
    | "print" :: "dependency-graph" :: _ -> ()
    | "print" :: "preprocessed" :: _ -> ()
    | "print" :: _ ->
      failwith "DO NOT PRINT ASTs IN EXPECT TESTS: PLEASE USE src/test/ast_production.ml"
    | _ -> ()
  in
  let exit_code = run_ligo args in
  if exit_code <> 0 then raise Should_exit_good else ()


let run_ligo_bad args =
  let exit_code = run_ligo args in
  if exit_code = 0 then raise Should_exit_bad else ()

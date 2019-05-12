open! Trace

let test name f =
  Alcotest.test_case name `Quick @@ fun () ->
  let result =
    trace (fun () -> error (thunk "running test") (fun () -> name) ()) @@
    f () in
  match result with
  | Ok ((), annotations) -> ignore annotations; ()
  | Errors errs ->
      Format.printf "Errors : {\n%a}\n%!" errors_pp (List.rev (List.rev_map (fun f -> f ()) errs)) ;
      raise Alcotest.Test_error

open Ast_simplified.Combinators

let expect ?options program entry_point input expecter =
  let%bind result =
    let run_error =
      let title () = "expect run" in
      let content () = Format.asprintf "Entry_point: %s" entry_point in
      error title content in
    trace run_error @@
    Ligo.easy_run_typed_simplified ~debug_michelson:false ?options entry_point program input in
  expecter result

let expect_eq ?options program entry_point input expected =
  let expecter = fun result ->
    let expect_error =
      let title () = "expect result" in
      let content () = Format.asprintf "Expected %a, got %a"
          Ast_simplified.PP.value expected
          Ast_simplified.PP.value result in
      error title content in
    trace expect_error @@
    Ast_simplified.assert_value_eq (expected , result) in
  expect ?options program entry_point input expecter

let expect_evaluate program entry_point expecter =
  let error =
    let title () = "expect evaluate" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content in
  trace error @@
  let%bind result = Ligo.easy_evaluate_typed_simplified entry_point program in
  expecter result

let expect_eq_evaluate program entry_point expected =
  let expecter = fun result ->
    Ast_simplified.assert_value_eq (expected , result) in
  expect_evaluate program entry_point expecter

let expect_n_aux ?options lst program entry_point make_input make_expecter =
  let aux n =
    let input = make_input n in
    let expecter = make_expecter n in
    trace (simple_error ("expect_n " ^ (string_of_int n))) @@
    let result = expect ?options program entry_point input expecter in
    result
  in
  let%bind _ = bind_map_list aux lst in
  ok ()

let expect_eq_n_aux ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace (simple_error ("expect_eq_n " ^ (string_of_int n))) @@
    let result = expect_eq ?options program entry_point input expected in
    result
  in
  let%bind _ = bind_map_list aux lst in
  ok ()

let expect_eq_n ?options = expect_eq_n_aux ?options [0 ; 2 ; 42 ; 163 ; -1]
let expect_eq_n_pos ?options = expect_eq_n_aux ?options [0 ; 2 ; 42 ; 163]
let expect_eq_n_strict_pos ?options = expect_eq_n_aux ?options [2 ; 42 ; 163]
let expect_eq_n_pos_small ?options = expect_eq_n_aux ?options [0 ; 2 ; 10]
let expect_eq_n_strict_pos_small ?options = expect_eq_n_aux ?options [2 ; 10]
let expect_eq_n_pos_mid = expect_eq_n_aux [0 ; 2 ; 10 ; 33]

let expect_n_pos_small ?options = expect_n_aux ?options [0 ; 2 ; 10]
let expect_n_strict_pos_small ?options = expect_n_aux ?options [2 ; 10]

let expect_eq_b program entry_point make_expected =
  let aux b =
    let input = e_a_bool b in
    let expected = make_expected b in
    expect_eq program entry_point input expected
  in
  let%bind _ = bind_map_list aux [false ; true] in
  ok ()

let expect_eq_n_int a b c =
  expect_eq_n a b e_a_int (fun n -> e_a_int (c n))

let expect_eq_b_bool a b c =
  let open Ast_simplified.Combinators in
  expect_eq_b a b (fun bool -> e_a_bool (c bool))

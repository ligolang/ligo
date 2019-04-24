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

let expect program entry_point input expected =
  let error =
    let title () = "expect run" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content in
  trace error @@
  let%bind result = Ligo.easy_run_typed_simplified entry_point program input in
  Ast_simplified.assert_value_eq (expected , result)

let expect_evaluate program entry_point expected =
  let error =
    let title () = "expect evaluate" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content in
  trace error @@
  let%bind result = Ligo.easy_evaluate_typed_simplified entry_point program in
  Ast_simplified.assert_value_eq (expected , result)

let expect_n_aux lst program entry_point make_input make_expected =
  Format.printf "expect_n aux\n%!" ;
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    let result = expect program entry_point input expected in
    result
  in
  let%bind _ = bind_map_list aux lst in
  ok ()

let expect_n = expect_n_aux [0 ; 2 ; 42 ; 163 ; -1]
let expect_n_pos = expect_n_aux [0 ; 2 ; 42 ; 163]
let expect_n_strict_pos = expect_n_aux [2 ; 42 ; 163]
let expect_n_pos_small = expect_n_aux [0 ; 2 ; 10]
let expect_n_strict_pos_small = expect_n_aux [2 ; 10]
let expect_n_pos_mid = expect_n_aux [0 ; 2 ; 10 ; 33]

let expect_b program entry_point make_expected =
  let aux b =
    let input = e_a_bool b in
    let expected = make_expected b in
    expect program entry_point input expected
  in
  let%bind _ = bind_map_list aux [false ; true] in
  ok ()

let expect_n_int a b c =
  expect_n a b e_a_int (fun n -> e_a_int (c n))

let expect_b_bool a b c =
  let open Ast_simplified.Combinators in
  expect_b a b (fun bool -> e_a_bool (c bool))

open! Trace

type test_case = unit Alcotest.test_case
type test =
  | Test_suite of (string * test list)
  | Test of test_case

let wrap_test name f =
  let result =
    trace (error (thunk "running test") (thunk name)) @@
    f () in
  match result with
  | Ok ((), annotations) -> ignore annotations; ()
  | Error err ->
    Format.printf "%a\n%!" Ligo.Display.error_pp (err ()) ;
    raise Alcotest.Test_error

let wrap_test_raw f =
  match f () with
  | Trace.Ok ((), annotations) -> ignore annotations; ()
  | Error err ->
    Format.printf "%a\n%!" Ligo.Display.error_pp (err ())

let test name f =
  Test (
    Alcotest.test_case name `Quick @@ fun () ->
    wrap_test name f
  )

let test_suite name lst = Test_suite (name , lst)

open Ast_simplified.Combinators

let expect ?input_to_value ?options program entry_point input expecter =
  let%bind result =
    let run_error =
      let title () = "expect run" in
      let content () = Format.asprintf "Entry_point: %s" entry_point in
      error title content in
    trace run_error @@
    Ligo.Run.run_simplityped ?input_to_value ~debug_michelson:true ?options program entry_point input in
  expecter result

let expect_fail ?options program entry_point input =
  let run_error =
    let title () = "expect run" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content
  in
  trace run_error @@
  Assert.assert_fail
  @@ Ligo.Run.run_simplityped ~debug_michelson:true ?options program entry_point input


let expect_eq ?input_to_value ?options program entry_point input expected =
  let expecter = fun result ->
    let expect_error =
      let title () = "expect result" in
      let content () = Format.asprintf "Expected %a, got %a"
          Ast_simplified.PP.expression expected
          Ast_simplified.PP.expression result in
      error title content in
    trace expect_error @@
    Ast_simplified.Misc.assert_value_eq (expected , result) in
  expect ?input_to_value ?options program entry_point input expecter

let expect_evaluate program entry_point expecter =
  let error =
    let title () = "expect evaluate" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content in
  trace error @@
  let%bind result = Ligo.Run.evaluate_simplityped ~debug_mini_c:true ~debug_michelson:true program entry_point in
  expecter result

let expect_eq_evaluate program entry_point expected =
  let expecter = fun result ->
    Ast_simplified.Misc.assert_value_eq (expected , result) in
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

let expect_eq_n_aux ?input_to_value ?options lst program entry_point make_input make_expected =
  let aux n =
    let input = make_input n in
    let expected = make_expected n in
    trace (simple_error ("expect_eq_n " ^ (string_of_int n))) @@
    let result = expect_eq ?input_to_value ?options program entry_point input expected in
    result
  in
  let%bind _ = bind_map_list_seq aux lst in
  ok ()

let expect_eq_n ?input_to_value ?options = expect_eq_n_aux ?input_to_value ?options [0 ; 1 ; 2 ; 42 ; 163 ; -1]
let expect_eq_n_pos ?input_to_value ?options = expect_eq_n_aux ?input_to_value ?options [0 ; 1 ; 2 ; 42 ; 163]
let expect_eq_n_strict_pos ?input_to_value ?options = expect_eq_n_aux ?input_to_value ?options [2 ; 42 ; 163]
let expect_eq_n_pos_small ?input_to_value ?options = expect_eq_n_aux ?input_to_value ?options [0 ; 1 ; 2 ; 10]
let expect_eq_n_strict_pos_small ?input_to_value ?options = expect_eq_n_aux ?input_to_value ?options [1 ; 2 ; 10]
let expect_eq_n_pos_mid ?input_to_value = expect_eq_n_aux ?input_to_value [0 ; 1 ; 2 ; 10 ; 33]

let expect_n_pos_small ?options = expect_n_aux ?options [0 ; 2 ; 10]
let expect_n_strict_pos_small ?options = expect_n_aux ?options [2 ; 10]

let expect_eq_b program entry_point make_expected =
  let aux b =
    let input = e_bool b in
    let expected = make_expected b in
    expect_eq program entry_point input expected
  in
  let%bind _ = bind_map_list_seq aux [false ; true] in
  ok ()

let expect_eq_n_int a b c =
  expect_eq_n a b e_int (fun n -> e_int (c n))

let expect_eq_b_bool a b c =
  let open Ast_simplified.Combinators in
  expect_eq_b a b (fun bool -> e_bool (c bool))


let rec test_height : test -> int = fun t ->
  match t with
  | Test _ -> 1
  | Test_suite (_ , lst) -> (List.fold_left max 1 @@ List.map test_height lst) + 1

let extract_test : test -> test_case = fun t ->
  match t with
  | Test tc -> tc
  | _ -> assert false

let extract_param : test -> (string * (string * test_case list) list) =
  let extract_element = extract_test in
  let extract_group : test -> (string * test_case list) = fun t ->
    match t with
    | Test tc -> ("isolated" , [ tc ])
    | Test_suite (name , lst) -> (name , List.map extract_element lst) in
  fun t ->
      match t with
      | Test tc -> ("" , [ ("isolated" , [ tc ] ) ])
      | Test_suite (name , lst) -> (name , List.map extract_group lst)

let x : _ -> (unit Alcotest.test) = fun x -> x

(*
  Alcotest.run parameters:
  string * (string * f list) list
*)

let rec run_test ?(prefix = "") : test -> unit = fun t ->
  match t with
  | Test case -> Alcotest.run "isolated test" [ ("" , [ case ]) ]
  | Test_suite (name , lst) -> (
      if (test_height t <= 3) then (
        let (name , tests) = extract_param t in
        Alcotest.run (prefix ^ name) tests
      ) else (
        List.iter (run_test ~prefix:(prefix ^ name ^ "_")) lst
      )
    )

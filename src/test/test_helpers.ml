open! Trace

type test_case = unit Alcotest.test_case
type test =
  | Test_suite of (string * test list)
  | Test of test_case

let rec error_pp out (e : error) =
    let open JSON_string_utils in
  let message =
    let opt = e |> member "message" |> string in
    let msg = Option.unopt ~default:"" opt in
    if msg = ""
    then ""
    else ": " ^ msg in
  let error_code =
    let error_code = e |> member "error_code" in
    match error_code with
    | `Null -> ""
    | _ -> " (" ^ (J.to_string error_code) ^ ")" in
  let title =
    let opt = e |> member "title" |> string in
    Option.unopt ~default:"" opt in
  let data =
    let data = e |> member "data" in
    match data with
    | `Null -> ""
    | _ -> " " ^ (J.to_string data) ^ "\n" in
  let infos =
    let infos = e |> member "infos" in
    match infos with
    | `Null -> ""
    | `List lst -> Format.asprintf "@[<v2>%a@]" PP_helpers.(list_sep error_pp (tag "@,")) lst
    | _ -> " " ^ (J.to_string infos) ^ "\n" in
  Format.fprintf out "%s%s%s.\n%s%s" title error_code message data infos


let test name f =
  Test (
    Alcotest.test_case name `Quick @@ fun () ->
    let result =
      trace (fun () -> error (thunk "running test") (thunk name) ()) @@
    f () in
    match result with
    | Ok ((), annotations) -> ignore annotations; ()
    | Error err ->
      Format.printf "Errors : {\n%a}\n%!" error_pp (err ()) ;
      raise Alcotest.Test_error
  )

let test_suite name lst = Test_suite (name , lst)
        
open Ast_simplified.Combinators

let expect ?options program entry_point input expecter =
  let%bind result =
    let run_error =
      let title () = "expect run" in
      let content () = Format.asprintf "Entry_point: %s" entry_point in
      error title content in
    trace run_error @@
    Ligo.Run.run_simplityped ~debug_michelson:true ?options program entry_point input in
  expecter result

let expect_eq ?options program entry_point input expected =
  let expecter = fun result ->
    let expect_error =
      let title () = "expect result" in
      let content () = Format.asprintf "Expected %a, got %a"
          Ast_simplified.PP.expression expected
          Ast_simplified.PP.expression result in
      error title content in
    trace expect_error @@
    Ast_simplified.Misc.assert_value_eq (expected , result) in
  expect ?options program entry_point input expecter

let expect_evaluate program entry_point expecter =
  let error =
    let title () = "expect evaluate" in
    let content () = Format.asprintf "Entry_point: %s" entry_point in
    error title content in
  trace error @@
  let%bind result = Ligo.Run.evaluate_simplityped program entry_point in
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

let expect_eq_n ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163 ; -1]
let expect_eq_n_pos ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 42 ; 163]
let expect_eq_n_strict_pos ?options = expect_eq_n_aux ?options [2 ; 42 ; 163]
let expect_eq_n_pos_small ?options = expect_eq_n_aux ?options [0 ; 1 ; 2 ; 10]
let expect_eq_n_strict_pos_small ?options = expect_eq_n_aux ?options [1 ; 2 ; 10]
let expect_eq_n_pos_mid = expect_eq_n_aux [0 ; 1 ; 2 ; 10 ; 33]

let expect_n_pos_small ?options = expect_n_aux ?options [0 ; 2 ; 10]
let expect_n_strict_pos_small ?options = expect_n_aux ?options [2 ; 10]

let expect_eq_b program entry_point make_expected =
  let aux b =
    let input = e_bool b in
    let expected = make_expected b in
    expect_eq program entry_point input expected
  in
  let%bind _ = bind_map_list aux [false ; true] in
  ok ()

let expect_eq_n_int a b c =
  expect_eq_n a b e_int (fun n -> e_int (c n))

let expect_eq_b_bool a b c =
  let open Ast_simplified.Combinators in
  expect_eq_b a b (fun bool -> e_bool (c bool))

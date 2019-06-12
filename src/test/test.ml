(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Test_helpers

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

let () =
  (* Printexc.record_backtrace true ; *)
  run_test @@ test_suite "LIGO" [
    Integration_tests.main ;
    Compiler_tests.main ;
    Transpiler_tests.main ;
    Typer_tests.main ;
    Heap_tests.main ;
    Coase_tests.main ;
    Vote_tests.main ;
    Bin_tests.main ;
  ] ;
  ()

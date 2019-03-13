(* -*- compile-command: "cd .. ; dune runtest" -*- *)

open Ligo_helpers.Trace
open Ligo

let test name f =
  Alcotest.test_case name `Quick @@ fun _sw ->
  match f () with
  | Ok () -> ()
  | Errors errs ->
    Format.printf "Errors : {\n%a}\n%!" errors_pp errs ;
    raise Alcotest.Test_error

open Mini_c
open Combinators

let simple_int_program body : program = [
  Fun("main", function_int body)
]

let run_int program n =
  Run.run program (`Int n) >>? function
  | `Int n -> ok n
  | _ -> simple_fail "run_int : output not int"

let neg () =
  let program : program = simple_int_program [
      assign_variable "output" @@ neg_int (var_int "input") ;
      assign_variable "output" @@ neg_int (var_int "output") ;
      assign_variable "output" @@ neg_int (var_int "output") ;
    ] in
  run_int program 42 >>? fun output ->
  Assert.assert_equal_int (-42) output >>? fun () ->
  ok ()

let multiple_variables () =
  let program = simple_int_program [
      assign_variable "a" @@ neg_int (var_int "input") ;
      assign_variable "b" @@ neg_int (var_int "a") ;
      assign_variable "c" @@ neg_int (var_int "b") ;
      assign_variable "d" @@ neg_int (var_int "c") ;
      assign_variable "output" @@ neg_int (var_int "d") ;
    ] in
  run_int program 42 >>? fun output ->
  Assert.assert_equal_int (-42) output >>? fun () ->
  ok ()

let arithmetic () =
  let expression = add_int (var_int "input") (neg_int (var_int "input")) in
  let program = simple_int_program [
      Assignment (Variable ("a", expression)) ;
      Assignment (Variable ("b", var_int "a")) ;
      Assignment (Variable ("output", var_int "b")) ;
    ] in
  let test n =
    run_int program n >>? fun output ->
    Assert.assert_equal_int 0 output >>? fun () ->
    ok ()
  in
  let%bind _assert = bind_list @@ List.map test [42 ; 150 ; 0 ; -42] in
  ok ()

let quote_ () =
  let program = simple_int_program [
      assign_function "f" @@ function_int [assign_variable "output" @@ add_int (var_int "input") (int 42)] ;
      assign_function "g" @@ function_int [assign_variable "output" @@ neg_int (var_int "input")] ;
      assign_variable "output" @@ apply_int (type_f_int @@ var "g") @@ apply_int (type_f_int @@ var "f") (var_int "input") ;
    ] in
  let%bind output = run_int program 42 in
  let%bind _ = Assert.assert_equal_int (-84) output in
  ok ()

let function_ () =
  let program = simple_int_program [
      assign_variable "a" @@ int 42 ;
      assign_function "f" @@ function_int [assign_variable "output" @@ add_int (var_int "input") (var_int "a")] ;
      let env = Environment.Small.of_list ["a", t_int] in
      assign_variable "output" @@ apply_int (type_closure_int env @@ var "f") (var_int "input") ;
    ] in
  let%bind output = run_int program 100 in
  let%bind _ = Assert.assert_equal_int 142 output in
  ok ()

let functions_ () =
  let program = simple_int_program [
      assign_variable "a" @@ int 42 ;
      assign_variable "b" @@ int 144 ;
      assign_function "f" @@ function_int [
        assign_variable "output" @@ add_int (var_int "input") (var_int "a")
      ] ;
      assign_function "g" @@ function_int [
        assign_variable "output" @@ add_int (var_int "input") (var_int "b")
      ] ;
      let env_f = Environment.Small.of_list ["a", t_int] in
      let env_g = Environment.Small.of_list ["b", t_int] in
      assign_variable "output" @@ add_int
        (apply_int (type_closure_int env_f @@ var "f") (var_int "input"))
        (apply_int (type_closure_int env_g @@ var "g") (var_int "input"))
    ] in
  let%bind output = run_int program 100 in
  let%bind _ = Assert.assert_equal_int 386 output in
  ok ()

let rich_function () =
  let program = simple_int_program [
      assign_variable "a" @@ int 42 ;
      assign_variable "b" @@ int 144 ;
      assign_function "f" @@ function_int [assign_variable "output" @@ add_int (var_int "a") (var_int "b")] ;
      let env = Environment.Small.of_list [("a", t_int) ; ("b", t_int)] in
      assign_variable "output" @@ apply_int (type_closure_int env @@ var "f") (var_int "input") ;
    ] in
  let test n =
    let%bind output = run_int program n in
    let%bind _ = Assert.assert_equal_int 186 output in
    ok () in
  let%bind _assert = bind_list @@ List.map test [42 ; 150 ; 0 ; -42] in
  ok ()

let main = "Mini_c", [
    test "basic.neg" neg ;
    test "basic.variables" multiple_variables ;
    test "basic.arithmetic" arithmetic ;
    test "basic.quote" quote_ ;
    test "basic.function" function_ ;
    test "basic.functions" functions_ ;
    test "basic.rich_function" rich_function ;
  ]

(* module Ligo = struct
 *   let parse_file (source:string) : Ligo.Untyped.Value.program result =
 *     let channel = open_in source in
 *     let lexbuf = Lexing.from_channel channel in
 *     specific_try (function
 *         | Parser.Error -> (
 *             let start = Lexing.lexeme_start_p lexbuf in
 *             let end_ = Lexing.lexeme_end_p lexbuf in
 *             let str = Format.sprintf
 *                 "Parse error at \"%s\" from (%d, %d) to (%d, %d)\n"
 *                 (Lexing.lexeme lexbuf)
 *                 start.pos_lnum (start.pos_cnum - start.pos_bol)
 *                 end_.pos_lnum (end_.pos_cnum - end_.pos_bol) in
 *             simple_error str
 *           )
 *         | Lexer.Unexpected_character s -> simple_error s
 *         | Lexer.Error _ -> simple_error "lexer error"
 *         | _ -> simple_error "unrecognized parse_ error"
 *       ) @@ (fun () -> Parser.main Lexer.token lexbuf) >>? fun program_ast ->
 *     ok program_ast
 *
 *   let run (source:string) (input:Ligo.Typed.Value.value) : Ligo.Typed.Value.value result =
 *     parse_file source >>? fun program_ast ->
 *     Ligo.Typecheck.typecheck_program program_ast >>? fun typed_program ->
 *     Ligo.Run.run typed_program input >>? fun output ->
 *     ok output
 *
 *   let assert_value_int : Ligo.Typed.Value.value -> int result = function
 *     | `Constant (`Int n) -> ok n
 *     | _ -> simple_fail "not an int"
 *
 *   let basic () : unit result =
 *     run "./contracts/toto.ligo" (Ligo.Typed.Value.int 42) >>? fun output ->
 *     assert_value_int output >>? fun output ->
 *     Assert.assert_equal_int 42 output >>? fun () ->
 *     ok ()
 *
 *   let display_basic () : unit result =
 *     parse_file "./contracts/toto.ligo" >>? fun program_ast ->
 *     Ligo.Typecheck.typecheck_program program_ast >>? fun typed_program ->
 *     Ligo.Transpile.program_to_michelson typed_program >>? fun node ->
 *     let node = Tezos_utils.Cast.flatten_node node in
 *     let str = Tezos_utils.Cast.node_to_string node in
 *     Format.printf "Program:\n%s\n%!" str ;
 *     ok ()
 *
 *   let main = "Ligo", [
 *       test "basic" basic ;
 *       test "basic.display" display_basic ;
 *     ]
 * end *)

let () =
  (* Printexc.record_backtrace true ; *)
  Alcotest.run "LIGO" [
    main ;
  ] ;
  ()

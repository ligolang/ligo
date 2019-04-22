open Trace
open Ligo
open Test_helpers

open Ast_simplified.Combinators

let function_ () : unit result =
  let%bind program = type_file "./contracts/function.ligo" in
  let make_expect = fun n -> n in
  expect_n_int program "main" make_expect

let complex_function () : unit result =
  let%bind program = type_file "./contracts/function-complex.ligo" in
  let make_expect = fun n -> (3 * n + 2) in
  expect_n_int program "main" make_expect

let variant () : unit result =
  let%bind program = type_file "./contracts/variant.ligo" in
  let%bind () =
    let expected = e_a_constructor "Foo" (e_a_int 42) in
    expect_evaluate program "foo" expected in
  let%bind () =
    let expected = e_a_constructor "Bar" (e_a_bool true) in
    expect_evaluate program "bar" expected in
  (* let%bind () =
   *   let make_expect = fun n -> (3 * n + 2) in
   *   expect_n_int program "fb" make_expect
   * in *)
  ok ()

let closure () : unit result =
  let%bind program = type_file "./contracts/closure.ligo" in
  let%bind () =
    let make_expect = fun n -> (2 * n) in
    expect_n_int program "foo" make_expect
  in
  let%bind _ =
    let make_expect = fun n -> (4 * n) in
    expect_n_int program "toto" make_expect
  in
  ok ()

let shadow () : unit result =
  let%bind program = type_file "./contracts/shadow.ligo" in
  let make_expect = fun _ -> 0 in
  expect_n_int program "foo" make_expect

let higher_order () : unit result =
  let%bind program = type_file "./contracts/high-order.ligo" in
  let make_expect = fun n -> n in
  expect_n_int program "foobar" make_expect

let shared_function () : unit result =
  let%bind program = type_file "./contracts/function-shared.ligo" in
  let%bind () =
    let make_expect = fun n -> (n + 1) in
    expect_n_int program "inc" make_expect
  in
  let%bind () =
    let make_expect = fun n -> (n + 2) in
    expect_n_int program "double_inc" make_expect
  in
  let%bind () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_n_int program "foo" make_expect
  in
  ok ()

let bool_expression () : unit result =
  let%bind program = type_file "./contracts/boolean_operators.ligo" in
  let%bind _ =
    let aux (name , f) = expect_b_bool program name f in
    bind_map_list aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
    ] in
  ok ()

let arithmetic () : unit result =
  let%bind program = type_file "./contracts/arithmetic.ligo" in
  let%bind _ =
    let aux (name , f) = expect_n_int program name f in
    bind_map_list aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
    ] in
  let%bind () = expect_n_pos program "int_op" e_a_nat e_a_int in
  ok ()

let unit_expression () : unit result =
  let%bind program = type_file "./contracts/unit.ligo" in
  expect_evaluate program "u" e_a_unit

let include_ () : unit result =
  let%bind program = type_file "./contracts/includer.ligo" in
  expect_evaluate program "bar" (e_a_int 144)

let record_ez_int names n =
  ez_e_a_record @@ List.map (fun x -> x, e_a_int n) names

let multiple_parameters () : unit result  =
  let%bind program = type_file "./contracts/multiple-parameters.ligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_a_int @@ make_output n in
    expect_n program name make_input make_output'
  in
  let%bind _ = bind_list @@ List.map aux [
      ("ab", record_ez_int ["a";"b"], fun n -> 2 * n) ;
      ("abcd", record_ez_int ["a";"b";"c";"d"], fun n -> 4 * n + 2) ;
      ("abcde", record_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ok ()

let record () : unit result  =
  let%bind program = type_file "./contracts/record.ligo" in
  let%bind () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_evaluate program "fb" expected
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_a_int (2 * n) in
    expect_n program "projection" make_input make_expected
  in
  let%bind () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_evaluate program "br" expected
  in
  ok ()

let tuple () : unit result  =
  let%bind program = type_file "./contracts/tuple.ligo" in
  let ez n =
    e_a_tuple (List.map e_a_int n) in
  let%bind () =
    let expected = ez [0 ; 0] in
    expect_evaluate program "fb" expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_a_int (2 * n) in
    expect_n program "projection" make_input make_expected
  in
  let%bind () =
    let expected = ez [23 ; 23 ; 23 ; 23 ; 23] in
    expect_evaluate program "br" expected
  in
  ok ()

let option () : unit result =
  let%bind program = type_file "./contracts/option.ligo" in
  let%bind () =
    let expected = e_a_some (e_a_int 42) in
    expect_evaluate program "s" expected
  in
  let%bind () =
    let expected = e_a_none t_int in
    expect_evaluate program "n" expected
  in
  ok ()

let map () : unit result =
  let%bind program = type_file "./contracts/map.ligo" in
  let ez lst =
    let open Ast_simplified.Combinators in
    let lst' = List.map (fun (x, y) -> e_a_int x, e_a_int y) lst in
    e_a_map lst' t_int t_int
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = e_a_int in
    expect_n program "gf" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez List.(map (fun x -> (x, x)) @@ range n) in
    let make_expected = e_a_nat in
    expect_n_strict_pos_small program "size_" make_input make_expected
  in
  let%bind () =
    let expected = ez [(23, 0) ; (42, 0)] in
    expect_evaluate program "fb" expected
  in
  let%bind () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_a_tuple [(e_a_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_n_pos_small program "set_" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_a_some @@ e_a_int 4 in
    expect_n program "get" make_input make_expected
  in
  let%bind () =
    let expected = ez @@ List.map (fun x -> (x, 23)) [144 ; 51 ; 42 ; 120 ; 421] in
    expect_evaluate program "bm" expected
  in
  let%bind () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect program "rm" input expected
  in
  ok ()

let list () : unit result =
  let%bind program = type_file "./contracts/list.ligo" in
  let ez lst =
    let lst' = List.map e_a_int lst in
    e_a_list lst' t_int
  in
  let%bind () =
    let make_input = fun n -> (ez @@ List.range n) in
    let make_expected = e_a_nat in
    expect_n_strict_pos_small program "size_" make_input make_expected
  in
  let%bind () =
    let expected = ez [23 ; 42] in
    expect_evaluate program "fb" expected
  in
  let%bind () =
    let expected = ez [144 ; 51 ; 42 ; 120 ; 421] in
    expect_evaluate program "bl" expected
  in
  ok ()

let condition () : unit result =
  let%bind program = type_file "./contracts/condition.ligo" in
  let make_input = e_a_int in
  let make_expected = fun n -> e_a_int (if n = 2 then 42 else 0) in
  expect_n program "main" make_input make_expected

let loop () : unit result =
  let%bind program = type_file "./contracts/loop.ligo" in
  let%bind () =
    let make_input = e_a_nat in
    let make_expected = e_a_nat in
    expect_n_pos program "dummy" make_input make_expected
  in
  let%bind () =
    let make_input = e_a_nat in
    let make_expected = e_a_nat in
    expect_n_pos_mid program "counter" make_input make_expected
  in
  let%bind () =
    let make_input = e_a_nat in
    let make_expected = fun n -> e_a_nat (n * (n + 1) / 2) in
    expect_n_pos_mid program "sum" make_input make_expected
  in
  ok()


let matching () : unit result =
  let%bind program = type_file "./contracts/match.ligo" in
  let%bind () =
    let make_input = e_a_int in
    let make_expected = fun n -> e_a_int (if n = 2 then 42 else 0) in
    expect_n program "match_bool" make_input make_expected
  in
  let%bind () =
    let make_input = e_a_int in
    let make_expected = fun n-> e_a_int (if n = 2 then 42 else 0) in
    expect_n program "match_expr_bool" make_input make_expected
  in
  let%bind () =
    let aux n =
      let input = match n with
        | Some s -> e_a_some (e_a_int s)
        | None -> e_a_none t_int in
      let expected = e_a_int (match n with
          | Some s -> s
          | None -> 23) in
      trace (simple_error (Format.asprintf "on input %a" PP_helpers.(option int) n)) @@
      expect program "match_option" input expected
    in
    bind_iter_list aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let%bind () =
    let aux n =
      let input = match n with
        | Some s -> e_a_some (e_a_int s)
        | None -> e_a_none t_int in
      let expected = e_a_int (match n with
          | Some s -> s
          | None -> 42) in
      trace (simple_error (Format.asprintf "on input %a" PP_helpers.(option int) n)) @@
      expect program "match_expr_option" input expected
    in
    bind_iter_list aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  ok ()

let declarations () : unit result =
  let%bind program = type_file "./contracts/declarations.ligo" in
  let make_input = e_a_int in
  let make_expected = fun n -> e_a_int (42 + n) in
  expect_n program "main" make_input make_expected

let quote_declaration () : unit result =
  let%bind program = type_file "./contracts/quote-declaration.ligo" in
  let make_input = e_a_int in
  let make_expected = fun n -> e_a_int (42 + 2 * n) in
  expect_n program "main" make_input make_expected

let quote_declarations () : unit result =
  let%bind program = type_file "./contracts/quote-declarations.ligo" in
  let make_input = e_a_int in
  let make_expected = fun n -> e_a_int (74 + 2 * n) in
  expect_n program "main" make_input make_expected

let counter_contract () : unit result =
  let%bind program = type_file "./contracts/counter.ligo" in
  let make_input = fun n-> e_a_pair (e_a_int n) (e_a_int 42) in
  let make_expected = fun n -> e_a_pair (e_a_list [] t_operation) (e_a_int (42 + n)) in
  expect_n program "main" make_input make_expected

let main = "Integration (End to End)", [
    test "function" function_ ;
    test "complex function" complex_function ;
    test "variant" variant ;
    test "closure" closure ;
    test "shared function" shared_function ;
    test "shadow" shadow ;
    test "multiple parameters" multiple_parameters ;
    test "bool" bool_expression ;
    test "arithmetic" arithmetic ;
    test "unit" unit_expression ;
    test "record" record ;
    test "tuple" tuple ;
    test "option" option ;
    test "map" map ;
    test "list" list ;
    test "condition" condition ;
    test "loop" loop ;
    test "matching" matching ;
    test "declarations" declarations ;
    test "quote declaration" quote_declaration ;
    test "quote declarations" quote_declarations ;
    test "#include directives" include_ ;
    test "counter contract" counter_contract ;
    test "higher order" higher_order ;
  ]

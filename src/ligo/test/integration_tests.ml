open Ligo_helpers.Trace
open Ligo
open Test_helpers

let pass (source:string) : unit result =
  let%bind raw =
    trace (simple_error "parsing") @@
    parse_file source in
  let%bind simplified =
    trace (simple_error "simplifying") @@
    simplify raw in
  let%bind typed =
    trace (simple_error "typing") @@
    type_ simplified in
  let%bind _mini_c =
    trace (simple_error "transpiling") @@
    transpile typed in
  ok ()

let basic () : unit result =
  pass "./contracts/toto.ligo"

let function_ () : unit result =
  let%bind _ = pass "./contracts/function.ligo" in
  let%bind _ = easy_run_main "./contracts/function.ligo" "2" in
  ok ()

let complex_function () : unit result =
  let%bind program = type_file "./contracts/function-complex.ligo" in
  let aux n =
    let open AST_Typed.Combinators in
    let input = a_int n in
    let%bind result = easy_run_main_typed program input in
    let%bind result' =
      trace (simple_error "bad result") @@
      get_a_int result in
    Assert.assert_equal_int (3 * n + 2) result'
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 42 ; 163 ; -1] in
  ok ()

let bool_expression () : unit result =
  let%bind program = type_file "./contracts/boolean_operators.ligo" in
  let aux (name, f) =
    let aux b =
      let open AST_Typed.Combinators in
      let input = a_bool b in
      let%bind result = easy_run_typed name program input in
      let%bind result' =
        trace (simple_error "bad result") @@
        get_a_bool result in
      Assert.assert_equal_bool (f b) result'
    in
    let%bind _ = bind_list
      @@ List.map aux [true;false] in
    ok ()
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
    ] in
  ok ()

let unit_expression () : unit result =
  let%bind program = type_file "./contracts/unit.ligo" in
  let open AST_Typed.Combinators in
  let%bind result = easy_evaluate_typed "u" program in
  let%bind () =
    trace (simple_error "result isn't unit") @@
    get_a_unit result in
  ok ()

let record_ez_int names n =
  let open AST_Typed.Combinators in
  a_record_ez @@ List.map (fun x -> x, a_int n) names

let multiple_parameters () : unit result  =
  let%bind program = type_file "./contracts/multiple-parameters.ligo" in
  let inputs = [0 ; 2 ; 42 ; 163 ; -1] in
  let aux (name, input_f, output_f) =
    let aux n =
      let input = input_f n in
      let%bind result = easy_run_typed name program input in
      let%bind result' = AST_Typed.Combinators.get_a_int result in
      let expected = output_f n in
      let%bind _ = Assert.assert_equal_int expected result' in
      ok ()
    in
    let%bind _ = bind_list @@ List.map aux inputs in
    ok ()
  in
  let%bind _ = bind_list @@ List.map aux [
      ("ab", record_ez_int ["a";"b"], fun n -> 2 * n) ;
      ("abcd", record_ez_int ["a";"b";"c";"d"], fun n -> 4 * n + 2) ;
      ("abcde", record_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ok ()

let record () : unit result  =
  let%bind program = type_file "./contracts/record.ligo" in
  let%bind _foobar =
    let%bind result = easy_evaluate_typed "fb" program in
    let expect = record_ez_int ["foo";"bar"] 0 in
    AST_Typed.assert_value_eq (expect, result)
  in
  let%bind _projection =
    let aux n =
      let input = record_ez_int ["foo";"bar"] n in
      let%bind result = easy_run_typed "projection" program input in
      let expect = AST_Typed.Combinators.a_int (2 * n) in
      AST_Typed.assert_value_eq (expect, result)
    in
    bind_list @@ List.map aux [0 ; -42 ; 144]
  in
  let%bind _big =
    let%bind result = easy_evaluate_typed "br" program in
    let expect = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    AST_Typed.assert_value_eq (expect, result)
  in
  ok ()

let condition () : unit result =
  let%bind program = type_file "./contracts/condition.ligo" in
  let aux n =
    let open AST_Typed.Combinators in
    let input = a_int n in
    let%bind result = easy_run_main_typed program input in
    let%bind result' =
      trace (simple_error "bad result") @@
      get_a_int result in
    Assert.assert_equal_int (if n = 2 then 42 else 0) result'
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 42 ; 163 ; -1] in
  ok ()

let declarations () : unit result =
  let%bind program = type_file "./contracts/declarations.ligo" in
  let aux n =
    let open AST_Typed.Combinators in
    let input = a_int n in
    let%bind result = easy_run_main_typed program input in
    let%bind result' =
      trace (simple_error "bad result") @@
      get_a_int result in
    Assert.assert_equal_int (42 + n) result'
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 42 ; 163 ; -1] in
  ok ()

let quote_declaration () : unit result =
  let%bind program = type_file "./contracts/quote-declaration.ligo" in
  let aux n =
    let open AST_Typed.Combinators in
    let input = a_int n in
    let%bind result = easy_run_main_typed program input in
    let%bind result' =
      trace (simple_error "bad result") @@
      get_a_int result in
    Assert.assert_equal_int result' (42 + 2 * n)
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 42 ; 163 ; -1] in
  ok ()

let quote_declarations () : unit result =
  let%bind program = type_file "./contracts/quote-declarations.ligo" in
  let aux n =
    let open AST_Typed.Combinators in
    let input = a_int n in
    let%bind result = easy_run_main_typed program input in
    let%bind result' =
      trace (simple_error "bad result") @@
      get_a_int result in
    Assert.assert_equal_int result' (74 + 2 * n)
  in
  let%bind _ = bind_list
    @@ List.map aux
    @@ [0 ; 2 ; 42 ; 163 ; -1] in
  ok ()


let main = "Integration (End to End)", [
    test "basic" basic ;
    test "function" function_ ;
    test "complex function" complex_function ;
    test "bool" bool_expression ;
    test "unit" unit_expression ;
    test "record" record ;
    test "multiple parameters" multiple_parameters ;
    test "condition" condition ;
    test "declarations" declarations ;
    test "quote declaration" quote_declaration ;
    test "quote declarations" quote_declarations ;
  ]

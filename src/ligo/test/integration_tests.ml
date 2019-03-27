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

let multiple_parameters () : unit result  =
  let%bind program = type_file "./contracts/multiple-parameters.ligo" in
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
    test "bool" bool_expression ;
    test "function" function_ ;
    test "complex function" complex_function ;
    test "multiple parameters" multiple_parameters ;
    test "condition" condition ;
    test "declarations" declarations ;
    test "quote declaration" quote_declaration ;
    test "quote declarations" quote_declarations ;
  ]

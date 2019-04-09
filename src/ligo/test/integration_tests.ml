open Trace
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
    let input = e_a_int n in
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
      let input = e_a_bool b in
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

let include_ () : unit result =
  let%bind program = type_file "./contracts/includer.ligo" in
  let%bind result = easy_evaluate_typed "bar" program in
  let%bind n =
    trace (simple_error "Include failed") @@
    AST_Typed.Combinators.get_a_int result in
  Assert.assert_equal_int 144 n

let record_ez_int names n =
  let open AST_Typed.Combinators in
  ez_e_a_record @@ List.map (fun x -> x, e_a_int n) names

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
      let expect = AST_Typed.Combinators.e_a_int (2 * n) in
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

let tuple () : unit result  =
  let%bind program = type_file "./contracts/tuple.ligo" in
  let ez n =
    let open AST_Typed.Combinators in
    e_a_tuple (List.map e_a_int n) in
  let%bind _foobar =
    trace (simple_error "foobar") (
      let%bind result = easy_evaluate_typed "fb" program in
      let expect = ez [0 ; 0] in
      AST_Typed.assert_value_eq (expect, result)
    )
  in
  let%bind _projection = trace (simple_error "projection") (
      let aux n =
        let input = ez [n ; n] in
        let%bind result = easy_run_typed "projection" program input in
        let expect = AST_Typed.Combinators.e_a_int (2 * n) in
        AST_Typed.assert_value_eq (expect, result)
      in
      bind_list @@ List.map aux [0 ; -42 ; 144]
    )
  in
  let%bind _big =
    let%bind result = easy_evaluate_typed "br" program in
    let expect = ez [23 ; 23 ; 23 ; 23 ; 23] in
    AST_Typed.assert_value_eq (expect, result)
  in
  ok ()

let option () : unit result =
  let%bind program = type_file "./contracts/option.ligo" in
  let open AST_Typed.Combinators in
  let%bind _some = trace (simple_error "some") @@
    let%bind result = easy_evaluate_typed "s" program in
    let expect = e_a_some (e_a_int 42) in
    AST_Typed.assert_value_eq (expect, result)
  in
  let%bind _none = trace (simple_error "none") @@
    let%bind result = easy_evaluate_typed "n" program in
    let expect = e_a_none (t_int ()) in
    AST_Typed.assert_value_eq (expect, result)
  in
  ok ()

let map () : unit result =
  let%bind program = type_file "./contracts/map.ligo" in
  let ez lst =
    let open AST_Typed.Combinators in
    let lst' = List.map (fun (x, y) -> e_a_int x, e_a_int y) lst in
    e_a_map lst' (t_int ()) (t_int ())
  in
  let%bind _get_force = trace (simple_error "get_force") @@
    let aux n =
      let input = ez [(23, n) ; (42, 4)] in
      let%bind result = easy_run_typed "gf" program input in
      let expect = AST_Typed.Combinators.(e_a_int n) in
      AST_Typed.assert_value_eq (expect, result)
    in
    bind_map_list aux [0 ; 42 ; 51 ; 421 ; -3]
  in
  let%bind _size = trace (simple_error "size") @@
    let aux n =
      let input = ez List.(map (fun x -> (x, x)) @@ range n) in
      let%bind result = easy_run_typed "size_" program input in
      let expect = AST_Typed.Combinators.(e_a_nat n) in
      AST_Typed.assert_value_eq (expect, result)
    in
    bind_map_list aux [1 ; 10 ; 3]
  in
  let%bind _foobar = trace (simple_error "foobar") @@
    let%bind result = easy_evaluate_typed "fb" program in
    let expect = ez [(23, 0) ; (42, 0)] in
    AST_Typed.assert_value_eq (expect, result)
  in
  let%bind _get = trace (simple_error "get") @@
    let aux n =
      let input = ez [(23, n) ; (42, 4)] in
      let%bind result = easy_run_typed "get" program input in
      let expect = AST_Typed.Combinators.(e_a_some @@ e_a_int 4) in
      AST_Typed.assert_value_eq (expect, result)
    in
    bind_map_list aux [0 ; 42 ; 51 ; 421 ; -3]
  in
  let%bind _bigmap = trace (simple_error "bigmap") @@
    let%bind result = easy_evaluate_typed "bm" program in
    let expect = ez @@ List.map (fun x -> (x, 23)) [144 ; 51 ; 42 ; 120 ; 421] in
    AST_Typed.assert_value_eq (expect, result)
  in
  ok ()

let condition () : unit result =
  let%bind program = type_file "./contracts/condition.ligo" in
  let aux n =
    let open AST_Typed.Combinators in
    let input = e_a_int n in
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

let loop () : unit result =
  let%bind program = type_file "./contracts/loop.ligo" in
  let%bind _dummy = trace (simple_error "dummy") @@
    let aux n =
      let open AST_Typed.Combinators in
      let input = e_a_nat n in
      let%bind result = easy_run_typed "dummy" program input in
      let expected = e_a_nat n in
      AST_Typed.assert_value_eq (expected, result)
    in
    let%bind _ = bind_list
      @@ List.map aux
      @@ [0 ; 2 ; 42 ; 163] in
    ok ()
  in
  let%bind _counter = trace (simple_error "counter") @@
    let aux n =
      let open AST_Typed.Combinators in
      let input = e_a_nat n in
      let%bind result = easy_run_typed "counter" program input in
      let expected = e_a_nat n in
      AST_Typed.assert_value_eq (expected, result)
    in
    let%bind _ = bind_list
      @@ List.map aux
      @@ [0 ; 2 ; 42 ; 12] in
    ok ()
  in
  let%bind _sum = trace (simple_error "sum") @@
    let aux n =
      let open AST_Typed.Combinators in
      let input = e_a_nat n in
      let%bind result = easy_run_typed "sum" program input in
      let expected = e_a_nat (n * (n + 1) / 2) in
      AST_Typed.assert_value_eq (expected, result)
    in
    let%bind _ = bind_list
      @@ List.map aux
      @@ [0 ; 2 ; 42 ; 12] in
    ok ()
  in
  ok()


let matching () : unit result =
  let%bind program = type_file "./contracts/match.ligo" in
  let%bind _bool =
    let aux n =
      let open AST_Typed.Combinators in
      let input = e_a_int n in
      let%bind result = easy_run_typed "match_bool" program input in
      let%bind result' =
        trace (simple_error "bad result") @@
        get_a_int result in
      Assert.assert_equal_int (if n = 2 then 42 else 0) result'
    in
    let%bind _ = bind_list
      @@ List.map aux
      @@ [0 ; 2 ; 42 ; 163 ; -1] in
    ok ()
  in
  let%bind _expr_bool =
    let aux n =
      let open AST_Typed.Combinators in
      let input = e_a_int n in
      let%bind result = easy_run_typed "match_expr_bool" program input in
      let%bind result' =
        trace (simple_error "bad result") @@
        get_a_int result in
      Assert.assert_equal_int (if n = 2 then 42 else 0) result'
    in
    let%bind _ = bind_list
      @@ List.map aux
      @@ [0 ; 2 ; 42 ; 163 ; -1] in
    ok ()
  in
  let%bind _option =
    let aux n =
      let open AST_Typed.Combinators in
      let input = match n with
        | Some s -> e_a_some (e_a_int s)
        | None -> e_a_none (t_int ()) in
      let%bind result = easy_run_typed "match_option" program input in
      let%bind result' =
        trace (simple_error "bad result") @@
        get_a_int result in
      Assert.assert_equal_int (match n with None -> 23 | Some s -> s) result'
    in
    let%bind _ = bind_list
      @@ List.map aux
      @@ [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None] in
    ok ()
  in
  ok ()

let declarations () : unit result =
  let%bind program = type_file "./contracts/declarations.ligo" in
  let aux n =
    let open AST_Typed.Combinators in
    let input = e_a_int n in
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
    let input = e_a_int n in
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
    let input = e_a_int n in
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
    test "tuple" tuple ;
    test "option" option ;
    test "map" map ;
    test "multiple parameters" multiple_parameters ;
    test "condition" condition ;
    test "loop" loop ;
    test "matching" matching ;
    test "declarations" declarations ;
    test "quote declaration" quote_declaration ;
    test "quote declarations" quote_declarations ;
    test "#include directives" include_ ;
  ]

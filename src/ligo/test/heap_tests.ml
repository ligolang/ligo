open Ligo_helpers.Trace
open Ligo
open Test_helpers

let get_program =
  let s = ref None in
  fun () -> match !s with
    | Some s -> ok s
    | None -> (
        let%bind program = type_file "./contracts/heap.ligo" in
        s := Some program ;
        ok program
      )

let a_heap content size =
  let open AST_Typed.Combinators in
  a_record_ez [
    ("content", content) ;
    ("size", size) ;
  ]

let a_heap_ez ?value_type (content:(int * AST_Typed.ae) list) =
  let open AST_Typed.Combinators in
  let content =
    let aux = fun (x, y) -> a_int x, y in
    List.map aux content in
  let value_type = match value_type, content with
    | None, hd :: _ -> (snd hd).type_annotation
    | Some s, _ -> s
    | _ -> raise (Failure "no value type and heap empty when building heap") in
  a_map content make_t_int value_type

let is_empty () : unit result =
  let%bind program = get_program () in
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

let main = "Heap (End to End)", [
    test "is_empty" is_empty ;
  ]

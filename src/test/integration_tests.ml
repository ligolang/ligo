open Trace
open Test_helpers

open Ast_simplified.Combinators

let mtype_file ?debug_simplify ?debug_typed = Ligo.Compile.Of_source.type_file ?debug_simplify ?debug_typed (Syntax_name "cameligo")
let type_file = Ligo.Compile.Of_source.type_file (Syntax_name "pascaligo")

let type_alias () : unit result =
  let%bind program = type_file "./contracts/type-alias.ligo" in
  expect_eq_evaluate program "foo" (e_int 23)

let function_ () : unit result =
  let%bind program = type_file "./contracts/function.ligo" in
  let make_expect = fun n -> n in
  expect_eq_n_int program "main" make_expect

let blockless () : unit result =
  let%bind program = type_file "./contracts/blockless.ligo" in
  let make_expect = fun n-> n + 10 in
  expect_eq_n_int program "blockless" make_expect

(* Procedures are not supported yet 
  let procedure () : unit result =
  let%bind program = type_file "./contracts/procedure.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int program "main" make_expect *)

let assign () : unit result =
  let%bind program = type_file "./contracts/assign.ligo" in
  let make_expect = fun n -> n + 1 in
  expect_eq_n_int program "main" make_expect

let annotation () : unit result =
  let%bind program = type_file "./contracts/annotation.ligo" in
  let%bind () =
    expect_eq_evaluate program "lst" (e_list [])
  in
  let%bind () =
    expect_eq_evaluate program "address" (e_address "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx")
  in
  ok ()

let complex_function () : unit result =
  let%bind program = type_file "./contracts/function-complex.ligo" in
  let make_expect = fun n -> (3 * n + 2) in
  expect_eq_n_int program "main" make_expect

let variant () : unit result =
  let%bind program = type_file "./contracts/variant.ligo" in
  let%bind () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate program "foo" expected in
  let%bind () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate program "bar" expected in
  let%bind () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate program "kee" expected in
  ok ()

let variant_mligo () : unit result =
  let%bind program = mtype_file "./contracts/variant.mligo" in
  let%bind () =
    let expected = e_constructor "Foo" (e_int 42) in
    expect_eq_evaluate program "foo" expected in
  let%bind () =
    let expected = e_constructor "Bar" (e_bool true) in
    expect_eq_evaluate program "bar" expected in
  let%bind () =
    let expected = e_constructor "Kee" (e_nat 23) in
    expect_eq_evaluate program "kee" expected in
  ok ()

let variant_matching () : unit result =
  let%bind program = type_file "./contracts/variant-matching.ligo" in
  let%bind () =
    let make_input = fun n -> e_constructor "Foo" (e_int n) in
    let make_expected = e_int in
    expect_eq program "fb" (make_input 0) (make_expected 0) >>? fun () ->
    expect_eq_n program "fb" make_input make_expected >>? fun () ->
    expect_eq program "fb" (e_constructor "Kee" (e_nat 50)) (e_int 23) >>? fun () ->
    expect_eq program "fb" (e_constructor "Bar" (e_bool true)) (e_int 42) >>? fun () ->
    ok ()
  in
  ok ()

let closure () : unit result =
  let%bind program = type_file "./contracts/closure.ligo" in
  let%bind program_1 = type_file "./contracts/closure-1.ligo" in
  let%bind program_2 = type_file "./contracts/closure-2.ligo" in
  let%bind program_3 = type_file "./contracts/closure-3.ligo" in
  let%bind _ =
    let make_expect = fun n -> (49 + n) in
    expect_eq_n_int program_3 "foobar" make_expect
  in
  let%bind _ =
    let make_expect = fun n -> (45 + n) in
    expect_eq_n_int program_2 "foobar" make_expect
  in
  let%bind () =
    let make_expect = fun n -> (2 * n) in
    expect_eq_n_int program_1 "foo" make_expect
  in
  let%bind _ =
    let make_expect = fun n -> (4 * n) in
    expect_eq_n_int program "toto" make_expect
  in
  ok ()

let shadow () : unit result =
  let%bind program = type_file "./contracts/shadow.ligo" in
  let make_expect = fun _ -> 0 in
  expect_eq_n_int program "foo" make_expect

let higher_order () : unit result =
  let%bind program = type_file "./contracts/high-order.ligo" in
  let make_expect = fun n -> n in
  let%bind _ = expect_eq_n_int program "foobar" make_expect in
  let%bind _ = expect_eq_n_int program "foobar2" make_expect in
  let%bind _ = expect_eq_n_int program "foobar3" make_expect in
  let%bind _ = expect_eq_n_int program "foobar4" make_expect in
  let%bind _ = expect_eq_n_int program "foobar5" make_expect in
  ok () 

let shared_function () : unit result =
  let%bind program = type_file "./contracts/function-shared.ligo" in
  Format.printf "inc\n" ;
  let%bind () =
    let make_expect = fun n -> (n + 1) in
    expect_eq_n_int program "inc" make_expect
  in
  Format.printf "double inc?\n" ;
  let%bind () =
    expect_eq program "double_inc" (e_int 0) (e_int 2)
  in
  Format.printf "double incd!\n" ;
  let%bind () =
    let make_expect = fun n -> (n + 2) in
    expect_eq_n_int program "double_inc" make_expect
  in
  Format.printf "foo\n" ;
  let%bind () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_eq program "foo" (e_int 0) (e_int @@ make_expect 0)
  in
  let%bind () =
    let make_expect = fun n -> (2 * n + 3) in
    expect_eq_n_int program "foo" make_expect
  in
  ok ()

let bool_expression () : unit result =
  let%bind program = type_file "./contracts/boolean_operators.ligo" in
  let%bind _ =
    let aux (name , f) = expect_eq_b_bool program name f in
    bind_map_list aux [
      ("or_true", fun b -> b || true) ;
      ("or_false", fun b -> b || false) ;
      ("and_true", fun b -> b && true) ;
      ("and_false", fun b -> b && false) ;
      ("not_bool", fun b -> not b) ;
    ] in
  ok ()

let arithmetic () : unit result =
  let%bind program = type_file "./contracts/arithmetic.ligo" in
  let%bind _ =
    let aux (name , f) = expect_eq_n_int program name f in
    bind_map_list aux [
      ("plus_op", fun n -> (n + 42)) ;
      ("minus_op", fun n -> (n - 42)) ;
      ("times_op", fun n -> (n * 42)) ;
      ("neg_op", fun n -> (-n)) ;
    ] in
  let%bind () = expect_eq_n_pos program "int_op" e_nat e_int in
  let%bind () = expect_eq_n_pos program "mod_op" e_int (fun n -> e_nat (n mod 42)) in
  let%bind () = expect_eq_n_pos program "div_op" e_int (fun n -> e_int (n / 2)) in
  ok ()

let bitwise_arithmetic () : unit result =
  let%bind program = type_file "./contracts/bitwise_arithmetic.ligo" in
  let%bind () = expect_eq program "or_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 3) (e_nat 7) in
  let%bind () = expect_eq program "or_op" (e_nat 2) (e_nat 6) in
  let%bind () = expect_eq program "or_op" (e_nat 14) (e_nat 14) in
  let%bind () = expect_eq program "or_op" (e_nat 10) (e_nat 14) in
  let%bind () = expect_eq program "and_op" (e_nat 7) (e_nat 7) in
  let%bind () = expect_eq program "and_op" (e_nat 3) (e_nat 3) in
  let%bind () = expect_eq program "and_op" (e_nat 2) (e_nat 2) in
  let%bind () = expect_eq program "and_op" (e_nat 14) (e_nat 6) in
  let%bind () = expect_eq program "and_op" (e_nat 10) (e_nat 2) in
  let%bind () = expect_eq program "xor_op" (e_nat 0) (e_nat 7) in
  let%bind () = expect_eq program "xor_op" (e_nat 7) (e_nat 0) in
  ok ()

let string_arithmetic () : unit result =
  let%bind program = type_file "./contracts/string_arithmetic.ligo" in
  let%bind () = expect_eq program "concat_op" (e_string "foo") (e_string "foototo") in
  let%bind () = expect_eq program "concat_op" (e_string "") (e_string "toto") in
  let%bind () = expect_eq program "slice_op" (e_string "tata") (e_string "at") in
  let%bind () = expect_eq program "slice_op" (e_string "foo") (e_string "oo") in
  let%bind () = expect_fail program "slice_op" (e_string "ba") in
  ok ()

let bytes_arithmetic () : unit result =
  let%bind program = type_file "./contracts/bytes_arithmetic.ligo" in
  let%bind foo = e_bytes "0f00" in
  let%bind foototo = e_bytes "0f007070" in
  let%bind toto = e_bytes "7070" in
  let%bind empty = e_bytes "" in
  let%bind tata = e_bytes "7a7a7a7a" in
  let%bind at = e_bytes "7a7a" in
  let%bind ba = e_bytes "ba" in
  let%bind () = expect_eq program "concat_op" foo foototo in
  let%bind () = expect_eq program "concat_op" empty toto in
  let%bind () = expect_eq program "slice_op" tata at in
  let%bind () = expect_fail program "slice_op" foo in
  let%bind () = expect_fail program "slice_op" ba in
  let%bind b1 = Run.Of_simplified.run_typed_program program "hasherman" foo in
  let%bind () = expect_eq program "hasherman" foo b1 in
  let%bind b3 = Run.Of_simplified.run_typed_program program "hasherman" foototo in
  let%bind () = Assert.assert_fail @@ Ast_simplified.Misc.assert_value_eq (b3 , b1) in
  ok ()

let set_arithmetic () : unit result =
  let%bind program = type_file "./contracts/set_arithmetic.ligo" in
  let%bind program_1 = type_file "./contracts/set_arithmetic-1.ligo" in
  let%bind () =
    expect_eq program_1 "iter_op"
      (e_set [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "add_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_syntax"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "remove_deep" 
      (e_pair 
         (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
         (e_nat 42))
      (e_pair 
        (e_set [e_string "foo" ; e_string "bar"])
        (e_nat 42))
  in
  let%bind () =
    expect_eq program "patch_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"; e_string "foobar"]) in
  let%bind () =
    expect_eq program "patch_op_deep"
      (e_pair
         (e_set [e_string "foo" ; e_string "bar"])
         (e_nat 42))
      (e_pair
         (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
         (e_nat 42)) in
  let%bind () =
    expect_eq program "patch_op_empty"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_set [e_string "foo" ; e_string "bar"]) in
  let%bind () =
    expect_eq program "mem_op"
      (e_set [e_string "foo" ; e_string "bar" ; e_string "foobar"])
      (e_bool true) in
  let%bind () =
    expect_eq program "mem_op"
      (e_set [e_string "foo" ; e_string "bar"])
      (e_bool false) in
  let%bind () =
    expect_eq program_1 "fold_op"
      (e_set [ e_int 4 ; e_int 10 ])
      (e_int 29)
  in
  ok ()

let unit_expression () : unit result =
  let%bind program = type_file "./contracts/unit.ligo" in
  expect_eq_evaluate program "u" (e_unit ())

let string_expression () : unit result =
  let%bind program = type_file "./contracts/string.ligo" in
  let%bind _ = expect_eq_evaluate program "s" (e_string "toto") in
  expect_eq_evaluate program "y" (e_string "foototobar")

let include_ () : unit result =
  let%bind program = type_file "./contracts/includer.ligo" in
  expect_eq_evaluate program "bar" (e_int 144)

let record_ez_int names n =
  ez_e_record @@ List.map (fun x -> x, e_int n) names

let tuple_ez_int names n =
  e_tuple @@ List.map (fun _ -> e_int n) names

let multiple_parameters () : unit result  =
  let%bind program = type_file "./contracts/multiple-parameters.ligo" in
  let aux ((name : string) , make_input , make_output) =
    let make_output' = fun n -> e_int @@ make_output n in
    expect_eq_n program name make_input make_output'
  in
  let%bind _ = bind_list @@ List.map aux [
      ("ab", tuple_ez_int ["a";"b"], fun n -> 2 * n) ;
      ("abcd", tuple_ez_int ["a";"b";"c";"d"], fun n -> 4 * n + 2) ;
      ("abcde", tuple_ez_int ["a";"b";"c";"d";"e"], fun n -> 2 * n + 3) ;
    ] in
  ok ()

let record () : unit result  =
  let%bind program = type_file "./contracts/record.ligo" in
  let%bind () =
    let expected = record_ez_int ["foo" ; "bar"] 0 in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let%bind () = expect_eq_evaluate program "a" (e_int 42) in
    let%bind () = expect_eq_evaluate program "b" (e_int 142) in
    let%bind () = expect_eq_evaluate program "c" (e_int 242) in
    ok ()
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["foo" ; "bar"] in
    let make_expected = fun n -> ez_e_record [("foo" , e_int 256) ; ("bar" , e_int n) ] in
    expect_eq_n program "modify" make_input make_expected
  in
  let%bind () =
    let make_input = record_ez_int ["a" ; "b" ; "c"] in
    let make_expected = fun n -> ez_e_record [
        ("a" , e_int n) ;
        ("b" , e_int 2048) ;
        ("c" , e_int n)
      ] in
    expect_eq_n program "modify_abc" make_input make_expected
  in
  let%bind () =
    let expected = record_ez_int ["a";"b";"c";"d";"e"] 23 in
    expect_eq_evaluate program "br" expected
  in
  ok ()

let tuple () : unit result  =
  let%bind program = type_file "./contracts/tuple.ligo" in
  let ez n =
    e_tuple (List.map e_int n) in
  let%bind () =
    let expected = ez [0 ; 0] in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; 2 * n ; n] in
    let make_expected = fun n -> e_int (2 * n) in
    expect_eq_n program "projection_abc" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq program "modify_abc" (make_input 12) (make_expected 12)
  in
  let%bind () =
    let make_input = fun n -> ez [n ; n ; n] in
    let make_expected = fun n -> ez [n ; 2048 ; n] in
    expect_eq_n program "modify_abc" make_input make_expected
  in
  let%bind () =
    let expected = ez [23 ; 23 ; 23 ; 23 ; 23] in
    expect_eq_evaluate program "br" expected
  in
  ok ()

let option () : unit result =
  let%bind program = type_file "./contracts/option.ligo" in
  let%bind () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate program "s" expected
  in
  let%bind () =
    let expected = e_typed_none t_int in
    expect_eq_evaluate program "n" expected
  in
  let%bind () =
    let expected = e_typed_none t_int in
    expect_eq program "assign" (e_int 12) expected
  in
  ok ()

let moption () : unit result =
  let%bind program = mtype_file "./contracts/option.mligo" in
  let%bind () =
    let expected = e_some (e_int 42) in
    expect_eq_evaluate program "s" expected
  in
  let%bind () =
    let expected = e_typed_none t_int in
    expect_eq_evaluate program "n" expected
  in
  ok ()

let map_ type_f path : unit result =
  let%bind program = type_f path in
  let ez lst =
    let open Ast_simplified.Combinators in
    let lst' = List.map (fun (x, y) -> e_int x, e_int y) lst in
    e_typed_map lst' t_int t_int
  in
  let%bind () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small program "set_" make_input make_expected
  in
  let%bind () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq program "rm" input expected
  in
  let%bind () =
    let input = ez [(0,0) ; (1,1) ; (2,2)] in
    let expected = ez [(0, 5) ; (1, 6) ; (2, 7)] in
    expect_eq program "patch_" input expected
  in
  let%bind () =
    let input = ez [(0,0) ; (1,1) ; (2,2)] in
    let expected = ez [(0,0) ; (1,1) ; (2,2)] in
    expect_eq program "patch_empty" input expected
  in
  let%bind () =
    let input = (e_pair
                   (ez [(0,0) ; (1,1) ; (2,2)])
                   (e_nat 10)) in
    let expected = (e_pair
                      (ez [(0,0) ; (1,9) ; (2,2)])
                      (e_nat 10)) in
    expect_eq program "patch_deep" input expected
  in
  let%bind () =
    let make_input = fun n -> ez List.(map (fun x -> (x, x)) @@ range n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small program "size_" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = e_int in
    expect_eq_n program "gf" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n program "get" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n program "get_" make_input make_expected
  in
  let%bind () = expect_eq_evaluate program "empty_map"
    (e_annotation (e_map []) (t_map t_int t_int)) in
  let%bind () =
    let expected = ez @@ List.map (fun x -> (x, 23)) [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate program "map1" expected
  in
  let%bind () =
    let expected = ez [(23, 0) ; (42, 0)] in
    expect_eq_evaluate program "map2" expected
  in
  let%bind () =
    let input = ez [(1 , 1) ; (2 , 2) ; (3 , 3) ] in
    let expected = e_unit () in
    expect_eq program "iter_op" input expected
  in
  let%bind () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = ez [(1 , 11) ; (2 , 21) ; (3 , 31) ] in
    expect_eq program "map_op" input expected
  in
  let%bind () =
    let input = ez [(1 , 10) ; (2 , 20) ; (3 , 30) ] in
    let expected = e_int 76 in
    expect_eq program "fold_op" input expected
  in
  let%bind () =
    let input = ez [(2 , 20) ; (42 , 10)] in
    let expected = ez [(2 , 20) ; (32 , 16) ] in
    expect_eq program "deep_op" input expected
  in
  ok ()

let big_map_ type_f path : unit result =
  let%bind program = type_f path in
  let ez lst =
    let open Ast_simplified.Combinators in
    let lst' = List.map (fun (x, y) -> e_int x, e_int y) lst in
    e_pair (e_typed_big_map lst' t_int t_int) (e_unit ())
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = e_int in
    expect_eq_n ~input_to_value:true program "gf" make_input make_expected
  in
  let%bind () =
    let make_input = fun n ->
      let m = ez [(23 , 0) ; (42 , 0)] in
      e_tuple [(e_int n) ; m]
    in
    let make_expected = fun n -> ez [(23 , n) ; (42 , 0)] in
    expect_eq_n_pos_small ?input_to_value:(Some true) program "set_" make_input make_expected
  in
  let%bind () =
    let make_input = fun n -> ez [(23, n) ; (42, 4)] in
    let make_expected = fun _ -> e_some @@ e_int 4 in
    expect_eq_n ?input_to_value:(Some true) program "get" make_input make_expected
  in
  let%bind () =
    let input = ez [(23, 23) ; (42, 42)] in
    let expected = ez [23, 23] in
    expect_eq ?input_to_value:(Some true) program "rm" input expected
  in
  ok ()


let map () : unit result = map_ type_file "./contracts/map.ligo"
let mmap () : unit result = map_ mtype_file "./contracts/map.mligo"
let big_map () : unit result = big_map_ type_file "./contracts/big_map.ligo"
let mbig_map () : unit result = big_map_ mtype_file "./contracts/big_map.mligo"


let list () : unit result =
  let%bind program = type_file "./contracts/list.ligo" in
  let ez lst =
    let lst' = List.map e_int lst in
    e_typed_list lst' t_int
  in
  let%bind () =
    let expected = ez [23 ; 42] in
    expect_eq_evaluate program "fb" expected
  in
  let%bind () =
    let expected = ez [144 ; 23 ; 42] in
    expect_eq_evaluate program "fb2" expected
  in
  let%bind () =
    let expected = ez [688 ; 144 ; 23 ; 42] in
    expect_eq_evaluate program "fb3" expected
  in
  let%bind () =
    let make_input = fun n -> (ez @@ List.range n) in
    let make_expected = e_nat in
    expect_eq_n_strict_pos_small program "size_" make_input make_expected
  in
  let%bind () =
    let expected = ez [144 ; 51 ; 42 ; 120 ; 421] in
    expect_eq_evaluate program "bl" expected
  in
  let%bind () =
    expect_eq program "iter_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_int 13)
  in
  let%bind () =
    expect_eq program "map_op"
      (e_list [e_int 2 ; e_int 4 ; e_int 7])
      (e_list [e_int 3 ; e_int 5 ; e_int 8])
  in
  ok ()

let condition () : unit result =
  let%bind program = type_file "./contracts/condition.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
  expect_eq_n program "main" make_input make_expected

let condition_simple () : unit result =
  let%bind program = type_file "./contracts/condition-simple.ligo" in
  let make_input = e_int in
  let make_expected = fun _ -> e_int 42 in
  expect_eq_n program "main" make_input make_expected

let loop () : unit result =
  let%bind program = type_file "./contracts/loop.ligo" in
  let%bind () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos program "dummy" make_input make_expected
  in
  let%bind () =
    let make_input = e_nat in
    let make_expected = e_nat in
    expect_eq_n_pos_mid program "counter" make_input make_expected
  in
  let%bind () =
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "while_sum" make_input make_expected
  in(* For loop is currently unsupported 
      
  let%bind () = 
    let make_input = e_nat in
    let make_expected = fun n -> e_nat (n * (n + 1) / 2) in
    expect_eq_n_pos_mid program "for_sum" make_input make_expected
  in *)
  ok ()

(* Don't know how to assert parse error happens in this test framework
let for_fail () : unit result =
  let%bind program = type_file "./contracts/for_fail.ligo" in
  let%bind () = expect_fail program "main" (e_nat 0)
  in ok () *)

let matching () : unit result =
  let%bind program = type_file "./contracts/match.ligo" in
  let%bind () =
    let make_input = e_int in
    let make_expected = fun n -> e_int (if n = 2 then 42 else 0) in
    expect_eq_n program "match_bool" make_input make_expected
  in
  let%bind () =
    let make_input = e_int in
    let make_expected = fun n-> e_int (if n = 2 then 42 else 0) in
    expect_eq_n program "match_expr_bool" make_input make_expected
  in
  let%bind () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none t_int in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 23) in
      trace (simple_error (Format.asprintf "on input %a" PP_helpers.(option int) n)) @@
      expect_eq program "match_option" input expected
    in
    bind_iter_list aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let%bind () =
    let aux n =
      let input = match n with
        | Some s -> e_some (e_int s)
        | None -> e_typed_none t_int in
      let expected = e_int (match n with
          | Some s -> s
          | None -> 42) in
      trace (simple_error (Format.asprintf "on input %a" PP_helpers.(option int) n)) @@
      expect_eq program "match_expr_option" input expected
    in
    bind_iter_list aux
      [Some 0 ; Some 2 ; Some 42 ; Some 163 ; Some (-1) ; None]
  in
  let%bind () =
    let aux lst = e_annotation (e_list @@ List.map e_int lst) (t_list t_int) in
    let%bind () = expect_eq program "match_expr_list" (aux [ 14 ; 2 ; 3 ]) (e_int 14) in
    let%bind () = expect_eq program "match_expr_list" (aux [ 13 ; 2 ; 3 ]) (e_int 13) in
    let%bind () = expect_eq program "match_expr_list" (aux []) (e_int (-1)) in
    ok ()
  in
  ok ()

let declarations () : unit result =
  let%bind program = type_file "./contracts/declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + n) in
  expect_eq program "main" (make_input 0) (make_expected 0) >>? fun () ->
  expect_eq_n program "main" make_input make_expected

let declaration_local () : unit result =
  let%bind program = type_file "./contracts/declaration-local.ligo" in
  let make_input = e_int in
  let make_expected = fun _ -> e_int 42 in
  expect_eq_n program "main" make_input make_expected

let quote_declaration () : unit result =
  let%bind program = type_file "./contracts/quote-declaration.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (42 + 2 * n) in
  expect_eq_n program "main" make_input make_expected

let quote_declarations () : unit result =
  let%bind program = type_file "./contracts/quote-declarations.ligo" in
  let make_input = e_int in
  let make_expected = fun n -> e_int (74 + 2 * n) in
  expect_eq_n program "main" make_input make_expected

let counter_contract () : unit result =
  let%bind program = type_file "./contracts/counter.ligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] t_operation) (e_int (42 + n)) in
  expect_eq_n program "main" make_input make_expected

let super_counter_contract () : unit result =
  let%bind program = type_file "./contracts/super-counter.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] t_operation) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let super_counter_contract_mligo () : unit result =
  let%bind program = mtype_file "./contracts/super-counter.mligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] t_operation) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let dispatch_counter_contract () : unit result =
  let%bind program = type_file "./contracts/dispatch-counter.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] t_operation) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let failwith_ligo () : unit result =
  let%bind program = type_file "./contracts/failwith.ligo" in
  let should_fail = expect_fail program "main" in
  let should_work input = expect_eq program "main" input (e_pair (e_typed_list [] t_operation) (e_unit ())) in
  let%bind _ = should_work (e_pair (e_constructor "Zero" (e_nat 0)) (e_unit ())) in
  let%bind _ = should_fail (e_pair (e_constructor "Zero" (e_nat 1)) (e_unit ())) in
  let%bind _ = should_work (e_pair (e_constructor "Pos" (e_nat 1)) (e_unit ())) in
  let%bind _ = should_fail (e_pair (e_constructor "Pos" (e_nat 0)) (e_unit ())) in
  let should_fail input = expect_fail program "foobar" (e_int input) in
  let should_work input n = expect_eq program "foobar" (e_int input) (e_int n) in
  let%bind () = should_fail 10 in
  let%bind () = should_fail @@ -10 in
  let%bind () = should_work 5 6 in
  ok ()

let failwith_mligo () : unit result =
  let%bind program = mtype_file "./contracts/failwith.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  expect_fail program "main" make_input

let assert_mligo () : unit result =
  let%bind program = mtype_file "./contracts/assert.mligo" in
  let make_input b = e_pair (e_bool b) (e_unit ()) in
  let make_expected = e_pair (e_typed_list [] t_operation) (e_unit ()) in
  let%bind _ = expect_fail program "main" (make_input false) in
  let%bind _ = expect_eq program "main" (make_input true) make_expected in
  ok ()

let guess_the_hash_mligo () : unit result =
  let%bind program = mtype_file "./contracts/new-syntax.mligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] t_operation) (e_int (42 + n)) in
  expect_eq_n program "main" make_input make_expected

let guess_string_mligo () : unit result =
  let%bind program = mtype_file "./contracts/guess_string.mligo" in
  let make_input = fun n -> e_pair (e_int n) (e_int 42) in
  let make_expected = fun n -> e_pair (e_typed_list [] t_operation) (e_int (42 + n))
  in expect_eq_n program "main" make_input make_expected

let basic_mligo () : unit result =
  let%bind typed = mtype_file "./contracts/basic.mligo" in
  let%bind result = Run.Of_typed.evaluate_entry typed "foo" in
  Ast_typed.assert_value_eq
    (Ast_typed.Combinators.e_a_empty_int (42 + 127), result)

let counter_mligo () : unit result =
  let%bind program = mtype_file "./contracts/counter.mligo" in
  let make_input n = e_pair (e_int n) (e_int 42) in
  let make_expected n = e_pair (e_typed_list [] t_operation) (e_int (42 + n)) in
  expect_eq_n program "main" make_input make_expected

let let_in_mligo () : unit result =
  let%bind program = mtype_file "./contracts/letin.mligo" in
  let make_input n = e_pair (e_int n) (e_pair (e_int 3) (e_int 5)) in
  let make_expected n =
    e_pair (e_typed_list [] t_operation) (e_pair (e_int (7+n)) (e_int (3+5)))
  in expect_eq_n program "main" make_input make_expected

let match_variant () : unit result =
  let%bind program = mtype_file "./contracts/match.mligo" in
  let make_input n =
    e_pair (e_constructor "Sub" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] t_operation) (e_int (3-n))
  in expect_eq_n program "main" make_input make_expected

let match_matej () : unit result =
  let%bind program = mtype_file "./contracts/match_bis.mligo" in
  let make_input n =
    e_pair (e_constructor "Decrement" (e_int n)) (e_int 3) in
  let make_expected n =
    e_pair (e_typed_list [] t_operation) (e_int (3-n))
  in expect_eq_n program "main" make_input make_expected

let mligo_list () : unit result =
  let%bind program = mtype_file "./contracts/list.mligo" in
  let aux lst = e_list @@ List.map e_int lst in
  let%bind () = expect_eq program "fold_op" (aux [ 1 ; 2 ; 3 ]) (e_int 16) in
  let%bind () =
    let make_input n =
      e_pair (e_list [e_int n; e_int (2*n)])
        (e_pair (e_int 3) (e_list [e_int 8])) in
    let make_expected n =
      e_pair (e_typed_list [] t_operation)
        (e_pair (e_int (n+3)) (e_list [e_int (2*n)]))
    in
    expect_eq_n program "main" make_input make_expected
  in
  let%bind () = expect_eq_evaluate program "x" (e_list []) in
  let%bind () = expect_eq_evaluate program "y" (e_list @@ List.map e_int [3 ; 4 ; 5]) in
  let%bind () = expect_eq_evaluate program "z" (e_list @@ List.map e_int [2 ; 3 ; 4 ; 5]) in
  let%bind () = expect_eq program "map_op" (aux [2 ; 3 ; 4 ; 5]) (aux [3 ; 4 ; 5 ; 6]) in
  let%bind () = expect_eq program "iter_op" (aux [2 ; 3 ; 4 ; 5]) (e_unit ()) in
  ok ()

let lambda_mligo () : unit result =
  let%bind program = mtype_file "./contracts/lambda.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected

let lambda_ligo () : unit result =
  let%bind program = type_file "./contracts/lambda.ligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected

let lambda2_mligo () : unit result =
  let%bind program = mtype_file "./contracts/lambda2.mligo" in
  let make_input = e_pair (e_unit ()) (e_unit ()) in
  let make_expected = (e_unit ()) in
  expect_eq program "main" make_input make_expected

let website1_ligo () : unit result =
  let%bind program = type_file "./contracts/website1.ligo" in
  let make_input = fun n-> e_pair (e_int n) (e_int 42) in
  let make_expected = fun _n -> e_pair (e_typed_list [] t_operation) (e_int (42 + 1)) in
  expect_eq_n program "main" make_input make_expected

let website2_ligo () : unit result =
  let%bind program = type_file "./contracts/website2.ligo" in
  let make_input = fun n ->
    let action = if n mod 2 = 0 then "Increment" else "Decrement" in
    e_pair (e_constructor action (e_int n)) (e_int 42) in
  let make_expected = fun n ->
    let op = if n mod 2 = 0 then (+) else (-) in
    e_pair (e_typed_list [] t_operation) (e_int (op 42 n)) in
  expect_eq_n program "main" make_input make_expected

let tez_ligo () : unit result =
  let%bind program = type_file "./contracts/tez.ligo" in
  let%bind _ = expect_eq_evaluate program "add_tez" (e_mutez 42) in
  let%bind _ = expect_eq_evaluate program "sub_tez" (e_mutez 1) in
  let%bind _ = expect_eq_evaluate program "not_enough_tez" (e_mutez 4611686018427387903) in
  let%bind _ = expect_eq_evaluate program "nat_mul_tez" (e_mutez 100) in
  let%bind _ = expect_eq_evaluate program "tez_mul_nat" (e_mutez 1000) in
  let%bind _ = expect_eq_evaluate program "tez_div_tez1" (e_nat 100) in
  let%bind _ = expect_eq_evaluate program "tez_div_tez2" (e_nat 1) in
  let%bind _ = expect_eq_evaluate program "tez_div_tez3" (e_nat 0) in
  let%bind _ = expect_eq_evaluate program "tez_mod_tez1" (e_mutez 0) in
  let%bind _ = expect_eq_evaluate program "tez_mod_tez2" (e_mutez 10) in
  let%bind _ = expect_eq_evaluate program "tez_mod_tez3" (e_mutez 100) in
  ok ()

let tez_mligo () : unit result =
  let%bind program = mtype_file "./contracts/tez.mligo" in
  let%bind _ = expect_eq_evaluate program "add_tez" (e_mutez 42) in
  let%bind _ = expect_eq_evaluate program "sub_tez" (e_mutez 1) in
  let%bind _ = expect_eq_evaluate program "not_enough_tez" (e_mutez 4611686018427387903) in
  let%bind _ = expect_eq_evaluate program "add_more_tez" (e_mutez 111111000) in
  ok ()

let main = test_suite "Integration (End to End)" [
    test "type alias" type_alias ;
    test "function" function_ ;
    test "blockless function" blockless;
    (* test "procedure"  procedure ; *)
    test "assign" assign ;
    test "declaration local" declaration_local ;
    test "complex function" complex_function ;
    test "closure" closure ;
    test "shared function" shared_function ;
    test "higher order" higher_order ;
    test "variant" variant ;
    test "variant (mligo)" variant_mligo ;
    test "variant matching" variant_matching ;
    test "tuple" tuple ;
    test "record" record ;
    test "condition simple" condition_simple ;
    test "condition" condition ;
    test "shadow" shadow ;
    test "annotation" annotation ;
    test "multiple parameters" multiple_parameters ;
    test "bool" bool_expression ;
    test "arithmetic" arithmetic ;
    test "bitiwse_arithmetic" bitwise_arithmetic ;
    test "string_arithmetic" string_arithmetic ;
    test "bytes_arithmetic" bytes_arithmetic ;
    test "set_arithmetic" set_arithmetic ;
    test "unit" unit_expression ;
    test "string" string_expression ;
    test "option" option ;
    test "option (mligo)" moption ;
    test "map" map ;
    test "map (mligo)" mmap ;
    test "big_map" big_map ;
    test "big_map (mligo)" mbig_map ;
    test "list" list ;
    test "loop" loop ;
    test "matching" matching ;
    test "declarations" declarations ;
    test "quote declaration" quote_declaration ;
    test "quote declarations" quote_declarations ;
    test "#include directives" include_ ;
    test "counter contract" counter_contract ;
    test "super counter contract" super_counter_contract ;
    test "super counter contract" super_counter_contract_mligo ;
    test "dispatch counter contract" dispatch_counter_contract ;
    test "basic (mligo)" basic_mligo ;
    test "counter contract (mligo)" counter_mligo ;
    test "let-in (mligo)" let_in_mligo ;
    test "match variant (mligo)" match_variant ;
    test "match variant 2 (mligo)" match_matej ;
    test "list matching (mligo)" mligo_list ;
    (* test "guess the hash mligo" guess_the_hash_mligo ; WIP? *)
    test "failwith ligo" failwith_ligo ;
    test "failwith mligo" failwith_mligo ;
    test "assert mligo" assert_mligo ;
    (* test "guess string mligo" guess_string_mligo ; WIP? *)
    test "lambda mligo" lambda_mligo ;
    test "lambda ligo" lambda_ligo ;
    (* test "lambda2 mligo" lambda2_mligo ; *)
    test "tez (ligo)" tez_ligo ;
    test "tez (mligo)" tez_mligo ;
    test "website1 ligo" website1_ligo ;
    test "website2 ligo" website2_ligo ;
  ]

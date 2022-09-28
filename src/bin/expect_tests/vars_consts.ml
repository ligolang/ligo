open Cli_expect

let bad_test s = (bad_test "")^"vars_consts/"^s
let good_test s = (test "")^"vars_consts/"^s

(* Negatives *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "match.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/match.ligo", line 9, characters 8-9:
      8 |     | Some (s) -> {
      9 |         s := 3;
     10 |         result := s; }

    Mutable variable "s" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "match.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/match.jsligo", line 7, characters 23-24:
      6 |   let store2 = match (action, {
      7 |     Add: (n: int) => { n = 42; return n; },
      8 |     Sub: (n: int) => { n = 42; return -n; }

    Mutable variable "n" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "assign_const_param.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_param.ligo", line 3, characters 4-5:
      2 |   {
      3 |     x := 4;
      4 |   } with x

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "assign_consts.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_consts.ligo", line 4, characters 4-5:
      3 |     const (x, y) = (4, 5);
      4 |     x := 1;
      5 |   } with x + y + z

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "assign_consts.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_consts.jsligo", line 3, characters 2-3:
      2 |   const [x, y] = [4, 5];
      3 |   x = 1;
      4 |   return (x + y + z);

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "assign_const_params.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_params.ligo", line 3, characters 4-5:
      2 |   {
      3 |     x := 4;
      4 |     y := 3;

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "capture_var_param.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/capture_var_param.ligo", line 3, characters 42-43:
      2 |   {
      3 |     function bar(const _ : unit) : int is x;
      4 |   } with bar

    Invalid capture of non-constant variable "x", declared at
    File "../../test/contracts/negative/vars_consts/capture_var_param.ligo", line 1, characters 17-18:
      1 | function foo(var x : int) : int is
      2 |   { |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "capture_var_params.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/capture_var_params.ligo", line 3, characters 42-43:
      2 |   {
      3 |     function bar(const _ : unit) : int is x + y;
      4 |   } with bar

    Invalid capture of mutable variable "x" |}]

(* Dead test -- Alistair. Since [@var] is no-longer permitted in CameLIGO *)
(* let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-core" ; (bad_test "capture_var_params.mligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/capture_var_params.mligo", line 4, characters 7-13:
      3 |     match p with
      4 |       x[@var], y ->
      5 |         let bar : unit -> int = fun (_ : unit) -> x + y in
    Ill-formed pattern matching.
    At this point, if the pattern is complete, an arrow '->' is expected,
    followed by an expression. |}] *)

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "assign_const_param.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_param.jsligo", line 5, characters 5-8:
      4 |      const age: int = 3; // does not give an error
      5 |      age = 42; // does give an error
      6 |      return age;

    Mutable variable "age" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "assign_const_param_2.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/assign_const_param_2.jsligo", line 2, characters 2-3:
      1 | let x = (a: int): int => {
      2 |   a = 42;
      3 |   return a;

    Mutable variable "a" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "multiple_vars_1.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_1.jsligo", line 4, characters 4-5:
      3 |     const [x,y] = [4,5];
      4 |     x = 2;
      5 |     y = 3;

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "multiple_vars_2.jsligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_2.jsligo", line 4, characters 44-45:
      3 |     let [x,y] = [4,5];
      4 |     let add = (_ : unit) : int => { return (x + y); };
      5 |     return add();

    Invalid capture of mutable variable "x" |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "multiple_vars_1.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_1.ligo", line 4, characters 4-5:
      3 |     const (x, y) = (4, 5);
      4 |     x := 2;
      5 |     y := 3;

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "multiple_vars_2.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/multiple_vars_2.ligo", line 4, characters 38-39:
      3 |     var (x, y) := (4, 5);
      4 |     function add(const _u : unit) is (x + y);
      5 |   } with add(unit)

    Invalid capture of mutable variable "x" |}]

let%expect_test _ =
  run_ligo_bad [ "print" ; "ast-typed" ; (bad_test "capture_assign.ligo") ] ;
  [%expect{|
    File "../../test/contracts/negative/vars_consts/capture_assign.ligo", line 5, characters 4-5:
      4 |     const y = 0;
      5 |     x := 6;
      6 |   } with unit;

    Mutable variable "x" not found. |}]

(* Positives *)

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "shadowing.ligo") ] ;
  [%expect{|
    const foo : int -> int = lambda (
      totoint)int return let mut toto : int = 2 in
                         let ()#2 : unit = totoint := 3 in !toto
    const bar : unit -> int = lambda (mut
      _uunit)int return let toto : int = 1 in
                        let mut toto : int = 2 in
                        let ()#3 : unit = totoint := 3 in !toto |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "func_const_var.ligo") ] ;
  [%expect{|
    const foo : int -> int -> int = lambda (
      xint)int -> int return let bar : int -> int = lambda (mut
                             yint)int return ADD(x , !y) in bar |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "func_same_const_var.ligo") ] ;
  [%expect{|
    const foo : int -> int = lambda (
      xint)int return let bar : int -> int = lambda (mut
                      xint)int return let ()#2 : unit = xint := ADD(!x , 1) in !x in
                      (bar)@(42) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "func_var_const.ligo") ] ;
  [%expect{|
    const foo : int -> int = lambda (mut
      xint)int return let bar : int -> int = lambda ( xint)int return x in
                      (bar)@(42) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "var_loop.ligo") ] ;
  [%expect{|
    const foo : int -> int = lambda (
      xint)int return let mut i : int = 0 in
                      let mut b : int = 5 in
                      let ()#2 : unit =
                        while AND(LT(!i , x) , GT(!b , 0)) do iint := ADD(!i , 1) in
                      !i |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "multiple_vars.ligo") ] ;
  [%expect{|
    const foo : unit -> int = lambda (
      _uunit)int return let match_#105 : ( int * int ) = ( 4 , 5 ) in
                         match match_#105 with
                          | ( x : int , y : int ) ->
                          let mut x : int = x in
                          let mut y : int = y in
                          let ()#3 : unit = xint := 2 in
                          let ()#2 : unit = yint := 3 in ADD(!x , !y)
    const bar : unit -> int = lambda (
      _uunit)int return let match_#107 : ( int * int ) = ( 4 , 5 ) in
                         match match_#107 with
                          | ( x : int , y : int ) ->
                          let add : unit -> int = lambda (
                          _uunit)int return ADD(x , y) in (add)@(unit) |}]

let%expect_test _ =
  run_ligo_good [ "print" ; "ast-typed" ; (good_test "multiple_vars.jsligo") ] ;
  [%expect{|
    const foo : unit -> int = lambda (
      _#2unit)int return let match_#108 : ( int * int ) = ( 4 , 5 ) in
                          match match_#108 with
                           | ( x : int , y : int ) ->
                           let mut x : int = x in
                           let mut y : int = y in
                           let ()#6 : unit = xint := 2 in
                           let ()#5 : unit = yint := 3 in
                           C_POLYMORPHIC_ADD(!x , !y)[@private]
    const bar : unit -> int = lambda (
      _#3unit)int return let match_#110 : ( int * int ) = ( 4 , 5 ) in
                          match match_#110 with
                           | ( x : int , y : int ) ->
                           let mut add : unit -> int = lambda (
                           _#4unit)int return C_POLYMORPHIC_ADD(x , y)[@private] in
                           (!add)@(unit)[@private] |}]


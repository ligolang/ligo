open Cli_expect

let bad_test s = bad_test "" ^ "vars_consts/" ^ s
let good_test s = test "" ^ "vars_consts/" ^ s

(* Negatives *)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "match.jsligo" ];
  [%expect
    {|
File "../../test/contracts/negative/vars_consts/match.jsligo", line 5, characters 27-28:
  4 |   let store2 = $match (action, {
  5 |     "Add": (n) => (() => { n = 42; return n; })(),
                                 ^
  6 |     "Sub": (n) => (() => { n = 42; return -n; })()

Mutable variable "n" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "assign_consts.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/assign_consts.jsligo", line 3, characters 2-3:
      2 |   const [x, y] = [4, 5];
      3 |   x = 1;
            ^
      4 |   return (x + y + z);

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "assign_const_param.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/assign_const_param.jsligo", line 5, characters 5-8:
      4 |      const age: int = 3; // does not give an error
      5 |      age = 42; // does give an error
               ^^^
      6 |      return age;

    Mutable variable "age" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "assign_const_param_2.jsligo" ];
  [%expect
    {|
File "../../test/contracts/negative/vars_consts/assign_const_param_2.jsligo", line 2, characters 2-3:
  1 | function x (a: int): int {
  2 |   a = 42;
        ^
  3 |   return a;
Mutable variable "a" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "multiple_vars_1.jsligo" ];
  [%expect
      {|
File "../../test/contracts/negative/vars_consts/multiple_vars_1.jsligo", line 3, characters 2-3:
  2 |   const [x,y] = [4,5];
  3 |   x = 2;
        ^
  4 |   y = 3;

Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "multiple_vars_2.jsligo" ];
  [%expect
      {|
File "../../test/contracts/negative/vars_consts/multiple_vars_2.jsligo", line 3, characters 34-35:
  2 |   let [x,y] = [4,5];
  3 |   const add = (_ : unit) : int => x + y;
                                        ^
  4 |   return add();

Invalid capture of mutable variable "x" |}]

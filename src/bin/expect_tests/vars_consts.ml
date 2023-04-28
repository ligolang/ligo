open Cli_expect

let bad_test s = bad_test "" ^ "vars_consts/" ^ s
let good_test s = test "" ^ "vars_consts/" ^ s

(* Negatives *)

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "match.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/match.jsligo", line 7, characters 23-24:
      6 |   let store2 = match (action, {
      7 |     Add: (n: int) => { n = 42; return n; },
                                 ^
      8 |     Sub: (n: int) => { n = 42; return -n; }

    Mutable variable "n" not found. |}]

(*
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "assign_const_param.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/assign_const_param.ligo", line 3, characters 4-5:
      2 |   {
      3 |     x := 4;
      4 |   } with x

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "assign_consts.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/assign_consts.ligo", line 4, characters 4-5:
      3 |     const (x, y) = (4, 5);
      4 |     x := 1;
      5 |   } with x + y + z

    Mutable variable "x" not found. |}]
*)

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

(*
let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "assign_const_params.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/assign_const_params.ligo", line 3, characters 4-5:
      2 |   {
      3 |     x := 4;
      4 |     y := 3;

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "capture_var_param.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/capture_var_param.ligo", line 3, characters 42-43:
      2 |   {
      3 |     function bar(const _ : unit) : int is x;
      4 |   } with bar

    Invalid capture of mutable variable "x" |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "capture_var_params.ligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/capture_var_params.ligo", line 3, characters 42-43:
      2 |   {
      3 |     function bar(const _ : unit) : int is x + y;
      4 |   } with bar

    Invalid capture of mutable variable "x" |}]
*)

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
      1 | let x = (a: int): int => {
      2 |   a = 42;
            ^
      3 |   return a;

    Mutable variable "a" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "multiple_vars_1.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/multiple_vars_1.jsligo", line 4, characters 4-5:
      3 |     const [x,y] = [4,5];
      4 |     x = 2;
              ^
      5 |     y = 3;

    Mutable variable "x" not found. |}]

let%expect_test _ =
  run_ligo_bad [ "print"; "ast-typed"; bad_test "multiple_vars_2.jsligo" ];
  [%expect
    {|
    File "../../test/contracts/negative/vars_consts/multiple_vars_2.jsligo", line 4, characters 44-45:
      3 |     let [x,y] = [4,5];
      4 |     let add = (_ : unit) : int => { return (x + y); };
                                                      ^
      5 |     return add();

    Invalid capture of mutable variable "x" |}]
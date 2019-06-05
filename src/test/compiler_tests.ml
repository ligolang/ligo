open Trace
open Ligo.Mini_c
open Combinators
open Test_helpers

let run_entry_int (e:anon_function) (n:int) : int result =
  let param : value = D_int n in
  let%bind result = Main.Run_mini_c.run_entry e param in
  match result with
  | D_int n -> ok n
  | _ -> simple_fail "result is not an int"

let identity () : unit result =
  let%bind f = basic_int_quote (e_var_int "input") in
  let%bind result = run_entry_int f 42 in
  let%bind _ = Assert.assert_equal_int ~msg:__LOC__ 42 result in
  ok ()

let multiple_vars () : unit result =
  let expr =
    e_let_int "a" t_int (e_var_int "input") @@
    e_let_int "b" t_int (e_var_int "input") @@
    e_let_int "c" t_int (e_var_int "a") @@
    e_let_int "output" t_int (e_var_int "c") @@
    e_var_int "output" in
  let%bind f = basic_int_quote expr in
  let%bind result = run_entry_int f 42 in
  let%bind _ = Assert.assert_equal_int ~msg:__LOC__ 42 result in
  ok ()

let main = test_suite "Compiler (from Mini_C)" [
    test "identity" identity ;
    test "multiple_vars" multiple_vars ;
  ]

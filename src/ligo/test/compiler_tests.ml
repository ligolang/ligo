open Trace
open Ligo.Mini_c
open Combinators
open Test_helpers

let run_entry_int (e:anon_function) (n:int) : int result =
  let param : value = D_int n in
  let%bind result = Run.run_entry e param in
  match result with
  | D_int n -> ok n
  | _ -> simple_fail "result is not an int"

let identity () : unit result =
  let e = basic_int_quote_env in
  let s = statement (S_assignment ("output", e_var_int "input" e)) e in
  let%bind b = block [s] in
  let%bind f = basic_int_quote b in
  let%bind result = run_entry_int f 42 in
  let%bind _ = Assert.assert_equal_int ~msg:__LOC__ 42 result in
  ok ()

let multiple_vars () : unit result =
  let e = basic_int_quote_env in
  (*
     Statements can change the environment, and you don't want to pass the new environment manually.
     [statements] deals with this and this is why those statements are parametrized over an environment.
     Yes. One could do a monad. Feel free when we have the time.
   *)
  let ss = statements [
      (fun e -> statement (S_assignment ("a", e_var_int "input" e)) e) ;
      (fun e -> statement (S_assignment ("b", e_var_int "input" e)) e) ;
      (fun e -> statement (S_assignment ("c", e_var_int "a" e)) e) ;
      (fun e -> statement (S_assignment ("output", e_var_int "c" e)) e) ;
    ] e in
  let%bind b = block ss in
  let%bind f = basic_int_quote b in
  let%bind result = run_entry_int f 42 in
  let%bind _ = Assert.assert_equal_int ~msg:__LOC__ 42 result in
  ok ()

let main = "Compiler (from Mini_C)", [
    test "identity" identity ;
    test "multiple_vars" multiple_vars ;
  ]

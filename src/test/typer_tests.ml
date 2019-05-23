open Trace
open Ligo.AST_Simplified
open Test_helpers

module Typed = Ligo.AST_Typed
module Typer = Ligo.Typer
module Simplified = Ligo.AST_Simplified

let int () : unit result =
  let open Combinators in
  let pre = e_int 32 in
  let open Typer in
  let e = Environment.full_empty in
  let%bind post = type_expression e pre in
  let open! Typed in
  let open Combinators in
  let%bind () = assert_type_value_eq (post.type_annotation, t_int ()) in
  ok ()

module TestExpressions = struct
  let test_expression ?(env = Typer.Environment.full_empty)
                      (expr : expression)
                      (test_expected_ty : Typed.tv) =
    let pre = expr in
    let open Typer in
    let open! Typed in
    let%bind post = type_expression env pre in
    let%bind () = assert_type_value_eq (post.type_annotation, test_expected_ty) in
    ok ()

  module I = Simplified.Combinators
  module O = Typed.Combinators
  module E = O

  let unit   () : unit result = test_expression I.(e_unit ())    O.(t_unit ())
  let int    () : unit result = test_expression I.(e_int 32)  O.(t_int ())
  let bool   () : unit result = test_expression I.(e_bool true)  O.(t_bool ())
  let string () : unit result = test_expression I.(e_string "s") O.(t_string ())
  let bytes  () : unit result = test_expression I.(e_bytes "b")  O.(t_bytes ())

  let lambda () : unit result =
    test_expression
      I.(e_lambda "x" (Some t_int) (Some t_int) (e_var "x"))
      O.(t_function (t_int ()) (t_int ()) ())

  let tuple () : unit result =
    test_expression
      I.(e_tuple [e_int 32; e_string "foo"])
      O.(t_tuple [t_int (); t_string ()] ())

  let constructor () : unit result =
    let variant_foo_bar =
      O.[("foo", t_int ()); ("bar", t_string ())]
    in test_expression
      ~env:(E.env_sum_type variant_foo_bar)
      I.(e_constructor "foo" (e_int 32))
      O.(make_t_ez_sum variant_foo_bar)

  let record () : unit result =
    test_expression
      I.(ez_e_record        [("foo", e_int 32);  ("bar", e_string "foo")])
      O.(make_t_ez_record [("foo", t_int ()); ("bar", t_string ())])

end
(* TODO: deep types (e.g. record of record)
   TODO: negative tests (expected type error) *)

let main = "Typer (from simplified AST)", [
    test "int" int ;
    test "unit"        TestExpressions.unit ;
    test "int2"        TestExpressions.int ;
    test "bool"        TestExpressions.bool ;
    test "string"      TestExpressions.string ;
    test "bytes"       TestExpressions.bytes ;
    test "tuple"       TestExpressions.tuple ;
    test "constructor" TestExpressions.constructor ;
    test "record"      TestExpressions.record ;
    test "lambda"      TestExpressions.lambda ;
  ]

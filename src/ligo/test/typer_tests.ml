open Ligo_helpers.Trace
open Ligo.AST_Simplified
open Test_helpers

module Typed = Ligo.AST_Typed
module Typer = Ligo.Typer
module Simplified = Ligo.AST_Simplified

let int () : unit result =
  let open Combinators in
  let pre = ae @@ number 32 in
  let open Typer in
  let e = Environment.empty in
  let%bind post = type_annotated_expression e pre in
  let open Typed in
  let open Combinators in
  let%bind () = assert_type_value_eq (post.type_annotation, make_t_int) in
  ok ()

module TestExpressions = struct
  let test_expression (expr : expression) (test_expected_ty : Typed.tv) =
    let open Typer in
    let open Typed in
    let pre = ae @@ expr in
    let e = Environment.empty in
    let%bind post = type_annotated_expression e pre in
    let%bind () = assert_type_value_eq (post.type_annotation, test_expected_ty) in
    ok ()

  open Simplified.Combinators
  open Typed.Combinators

  let unit   () : unit result = test_expression (unit ())    (make_t_unit)
  let int    () : unit result = test_expression (number 32)  (make_t_int)
  let bool   () : unit result = test_expression (bool true)  (make_t_bool)
  let string () : unit result = test_expression (string "s") (make_t_string)
  let bytes  () : unit result = test_expression (bytes "b")  (make_t_bytes)

  let tuple () : unit result =
    test_expression Simplified.Combinators.(tuple [
                         ae @@ number 32 ;
                         ae @@ string "foo" ;
                    ])
                    (make_t_tuple [
                         make_t_int ;
                         make_t_string ;
                    ])

  let record () : unit result =
    test_expression Simplified.Combinators.(record [
                         ("foo", ae @@ number 32) ;
                         ("bar", ae @@ string "foo") ;
                    ])
                    (make_t_ez_record [
                         ("foo", make_t_int) ;
                         ("bar", make_t_string) ;
                    ])

end
(* TODO: deep types (e.g. record of record)
   TODO: negative tests (expected type error) *)

let main = "Typer (from simplified AST)", [
    test "int" int ;
    test "unit"   TestExpressions.unit ;
    test "int2"   TestExpressions.int ;
    test "bool"   TestExpressions.bool ;
    test "string" TestExpressions.string ;
    test "bytes"  TestExpressions.bytes ;
    test "record" TestExpressions.record ;
  ]

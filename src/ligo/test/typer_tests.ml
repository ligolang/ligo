open Ligo_helpers.Trace
open Ligo.AST_Simplified
open Test_helpers

module Typed = Ligo.AST_Typed
module Typer = Ligo.Typer

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

let record () : unit result =
  let open Combinators in
  let pre = ae (record [
      ("foo", ae @@ number 32) ;
      ("bar", ae @@ number 23) ;
    ]) in
  let open Typer in
  let%bind post = type_annotated_expression Environment.empty pre in
  let open Typed in
  let open Combinators in
  let result_type = make_t_ez_record [
      ("foo", make_t_int) ;
      ("bar", make_t_int) ;
    ] in
  let%bind () = assert_type_value_eq (post.type_annotation, result_type) in
  ok ()


let main = "Typer (from simplified AST)", [
    test "int" int ;
    test "record" record ;
  ]

open Trace
open Ast_core
open Test_helpers
open Main_errors

module Typed = Ast_typed
module Typer = Typer
module Simplified = Ast_core

let int () : (unit, _) result =
  let open Combinators in
  let pre = e_int (Z.of_int 32) in
  let open Typer in
  let e = Environment.empty in
  let state = Typer.Solver.initial_state in
  let%bind (_, post, new_state) = trace typer_tracer @@ type_expression_subst (typer_switch ()) e state pre in
  let () = Typer.Solver.discard_state new_state in
  let open! Typed in
  let open Combinators in
  let%bind () = trace_option (test_internal __LOC__) @@ assert_type_expression_eq (post.type_expression, t_int ()) in
  ok ()

let init_env = Environment.default Environment.Protocols.current

module TestExpressions = struct
  let test_expression ?(env = init_env)
                      ?(state = Typer.Solver.initial_state)
                      (expr : expression)
                      (test_expected_ty : Typed.type_expression) =
    let pre = expr in
    let open Typer in
    let open! Typed in
    let%bind (_ , post , new_state) = trace typer_tracer @@ type_expression_subst (typer_switch ()) env state pre in
    let () = Typer.Solver.discard_state new_state in
    let () = Format.printf "RE.MILALA\n%a\n - \n%a\n" PP.type_expression post.type_expression PP.type_expression test_expected_ty in
    let%bind () = trace_option (test_internal __LOC__) @@ assert_type_expression_eq (post.type_expression, test_expected_ty) in
    ok ()

  module I = Simplified.Combinators
  module O = Typed.Combinators
  module E = Typed.Environment

  let unit   () : (unit, _) result = test_expression I.(e_unit ())    O.(t_unit ())
  let int    () : (unit, _) result = test_expression I.(e_int (Z.of_int 32))     O.(t_int ())
  let bool   () : (unit, _) result = test_expression I.(e_bool true)  O.(t_bool ())
  let string () : (unit, _) result = test_expression I.(e_string (Standard "s")) O.(t_string ())
  let bytes  () : (unit, _) result =
    let b = I.e_bytes_hex "0b" in
    test_expression b  O.(t_bytes ())

  let option () : (unit,_) result = test_expression I.(e_some @@ e_int Z.zero) O.(t_option @@ t_int ())
  let bytes_pack () : (unit,_) result = test_expression I.(e_constant C_BYTES_PACK [e_string @@ Standard "pack"]) O.(t_bytes ())
  let bytes_unpack () : (unit,_) result = test_expression I.(e_annotation (e_constant C_BYTES_UNPACK [e_bytes_string @@ "unpack"]) (t_option @@ t_bytes ())) O.(t_option @@ t_bytes ())

  let application () : (unit, _) result =
    test_expression
      I.(e_application (e_lambda_ez (Location.wrap @@ Var.of_name "x") ~ascr:(t_int ()) (Some (t_int ())) (e_var "x")) @@ e_int Z.one)
      O.(t_int ())

  let lambda () : (unit, _) result =
    test_expression
      I.(e_lambda_ez (Location.wrap @@ Var.of_name "x") ~ascr:(t_int ()) (Some (t_int ())) (e_var "x"))
      O.(t_function (t_int ()) (t_int ()) ())

  let let_in () : (unit, _) result =
    test_expression 
      I.(e_let_in_ez (Location.wrap @@ Var.of_name "x") false (e_int Z.zero) @@ e_var "x")
      O.(t_int ())

  let let_in_ascr () : (unit, _) result =
    test_expression 
      I.(e_let_in_ez (Location.wrap @@ Var.of_name "x") ~ascr:(t_int ()) false (e_int Z.zero) @@ e_var "x")
      O.(t_int ())

  let constructor () : (unit, _) result =
    let variant_foo_bar = Ast_typed.t_sum_ez [
        ("Foo", Typed.t_int () );
        ("Bar", Typed.t_string () ); ]
    in
    test_expression
      ~env:(E.add_type (Var.of_name "test_t") variant_foo_bar E.empty)
      I.(e_constructor "Foo" (e_int (Z.of_int 32)))
      variant_foo_bar

  let matching () : (unit, _) result =
    let variant_foo_bar = Ast_typed.t_sum_ez [
        ("Foo", Typed.t_int () );
        ("Bar", Typed.t_string () ); ]
    in
    test_expression
      ~env:(E.add_type (Var.of_name "test_t") variant_foo_bar E.empty)
      I.(e_matching (e_constructor "Foo" (e_int (Z.of_int 32)))
      @@ Match_variant [{constructor=Label "Foo"; proj=Location.wrap @@ Var.of_name "x"; body=e_var "x"};
                        {constructor=Label "Bar"; proj=Location.wrap @@ Var.of_name "_"; body=e_int Z.zero}]
      ) O.(t_int ())

  let record () : (unit, _) result =
    test_expression
      I.(e_record @@ LMap.of_list [(Label "foo", e_int (Z.of_int 32)); (Label "bar", e_string (Standard "foo"))])
      O.(make_t_ez_record [("foo", t_int ()); ("bar", t_string ())])

  let record_accessor () : (unit, _) result =
    test_expression
      I.(e_record_accessor (e_record @@ LMap.of_list [(Label "foo", e_int Z.zero)]) @@ Label "foo")
      O.(t_int ())

  let record_update () : (unit, _) result =
    test_expression
      I.(e_record_update (e_record @@ LMap.of_list [(Label "foo", e_int Z.zero); (Label "bar", e_string (Standard "foo"))]) (Label "foo") @@ e_int Z.one)
      O.(make_t_ez_record [("foo", t_int ()); ("bar", t_string ())])

  let tuple () : (unit, _) result =
    test_expression
      I.(e_record @@ LMap.of_list [(Label "0",e_int (Z.of_int 32)); (Label "1", e_string (Standard "foo"))])
      O.(make_t_ez_record [("0",t_int ()); ("1",t_string ())])
  
  let ascription () : (unit, _) result =
    test_expression
      I.(e_annotation (e_int Z.one) (t_int ()))
      O.(t_int ())


end
(* TODO: deep types (e.g. record of record)
   TODO: negative tests (expected type error) *)

let test enabled_for_typer_not_currently_in_use name f = enabled_for_typer_not_currently_in_use, test name f
let no = false
let y = true
let main = test_suite "Typer (from core AST)"
    @@ (fun lst -> List.map snd @@ match typer_switch () with Ast_typed.New -> List.filter fst lst | _ -> lst) @@ [
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "int" int ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "unit"            TestExpressions.unit ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "int2"            TestExpressions.int ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "bool"            TestExpressions.bool ; (* needs variants *)
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "string"          TestExpressions.string ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "bytes"           TestExpressions.bytes ;    
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "option"          TestExpressions.option ;    
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "bytes_pack"      TestExpressions.bytes_pack ;    
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "bytes_unpack"    TestExpressions.bytes_unpack ;    
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "application"     TestExpressions.application ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "lambda"          TestExpressions.lambda ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "let_in"          TestExpressions.let_in ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "let_in_ascr"     TestExpressions.let_in_ascr ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "constructor"     TestExpressions.constructor ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "matching"        TestExpressions.matching ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "record"          TestExpressions.record ;
    test no "record_accessor" TestExpressions.record_accessor ;
    test no "record_update"   TestExpressions.record_update ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "tuple"           TestExpressions.tuple ;
    test y (* enabled AND PASSES as of 02021-01-26 f6601c830 *) "ascription"      TestExpressions.ascription ;
  ]

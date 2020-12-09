open Trace

(* module Core = Typesystem.Core *)
open Ast_typed.Types
(* open Ast_typed.Reasons *)
open Ast_typed.Combinators

let test_err s = Main_errors.test_internal s
let tst_assert s p = Assert.assert_true (test_err s) p

let alias a b = make_sc_alias a b
let constructor tv tag args = make_sc_constructor tv tag args
let row tv = make_sc_row tv C_record []
let tc tc args = make_sc_typeclass tc args
let poly tv forall = make_sc_poly tv forall

module Test_vars = struct
  let tva : type_variable = Var.of_name "a"
  let tvb : type_variable = Var.of_name "b"
  let tvc : type_variable = Var.of_name "c"
end

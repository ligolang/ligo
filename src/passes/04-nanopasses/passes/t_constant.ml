open Ast_unified
open Pass_type

(* this pass is used for constant types taken from TString *)

let compile =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_string s } when String.is_prefix s ~prefix:"%constant:" ->
      t_constant ~loc (String.chop_prefix_exn s ~prefix:"%constant:")
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  `Cata { idle_cata_pass with ty_expr = pass_ty }


let reduction_check = Iter.defaults
let decompile = `None
let pass = morph ~name:__MODULE__ ~compile ~decompile ~reduction_check

open Unit_test_helpers

let%expect_test "tconstant_string" =
  {|
    ((PE_declaration
      (D_type_abstraction
        ((name string) (params ()) (type_expr (T_string %constant:string))))))
  |}
  |-> pass;
  [%expect
    {|
    ((PE_declaration
      (D_type_abstraction
       ((name string) (params ()) (type_expr (T_constant string))))))
  |}]

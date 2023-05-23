open Ast_unified
open Pass_type

(* this pass is used for constant types taken from TString *)
include Flag.No_arg ()

let name = __MODULE__

let compile ~raise:_ =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_string s }
      when String.is_prefix s ~prefix:"%constant:" ->
      t_constant ~loc (String.chop_prefix_exn s ~prefix:"%constant:")
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  Fold { idle_fold with ty_expr = pass_ty }


let reduction ~raise:_ = Iter.defaults

let decompile ~raise:_ =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_constant s } -> t_string ~loc ("%constant:" ^ s)
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  Fold { idle_fold with ty_expr = pass_ty }


open Unit_test_helpers.Ty_expr

let%expect_test "tconstant_string decompile" =
  {|
  (T_constant string)
  |} |-> decompile;
  [%expect {|
    (T_string %constant:string)
  |}]

let%expect_test "tconstant_string compile" =
  {|
    (T_string %constant:string)
  |} |-> compile;
  [%expect {|
    (T_constant string)
  |}]

open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(*
    T_app lhs should be a T_var , else error
  *)
include Flag.No_arg ()

let compile ~raise =
  let pass_ty : _ ty_expr_ -> ty_expr =
   fun t ->
    let return_self () = make_t ~loc:t.location t.wrap_content in
    match Location.unwrap t with
    | T_app { constr; type_args = _ } ->
      if Option.is_some (get_t_var_opt constr)
      then return_self ()
      else raise.error (expected_variable ({ fp = t } : ty_expr))
    | _ -> return_self ()
  in
  Fold { idle_fold with ty_expr = pass_ty }


let reduction ~raise =
  let open Location in
  let ty_expr : ty_expr ty_expr_ -> unit =
   fun t ->
    match t with
    | { wrap_content =
          T_app { constr = { fp = { wrap_content = T_var _; _ } }; type_args = _ }
      ; _
      } -> ()
    | { wrap_content = T_app { constr = { fp = { wrap_content = _; _ } }; type_args = _ }
      ; _
      } -> raise.error (wrong_reduction __MODULE__)
    | _ -> ()
  in
  { Iter.defaults with ty_expr }


let name = __MODULE__
let decompile ~raise:_ = Nothing

open Unit_test_helpers.Ty_expr

let%expect_test "compile_t_app_t_var" =
  {|
    (T_app
      ((constr (T_var my_var))
        (type_args ((T_var arg1) (T_var arg2)))))
  |}
  |-> compile;
  [%expect
    {| (T_app ((constr (T_var my_var)) (type_args ((T_var arg1) (T_var arg2))))) |}]

let%expect_test "compile_t_app_wrong" =
  {|
    (T_app
      ((constr (T_arg should_be_a_t_var))
       (type_args ((T_var arg1) (T_var arg2)))))
  |}
  |->! compile;
  [%expect
    {|
        Err : (Small_passes_expected_variable
                  (T_app
                      ((constr (T_arg should_be_a_t_var))
                          (type_args ((T_var arg1) (T_var arg2)))))) |}]

open Ast_unified
open Pass_type

(* this pass is stupid, lol :D *)
let name = __MODULE__

include Flag.No_arg ()

let compile ~raise:_ =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_arg s } ->
      let quote_var var = "'" ^ var in
      t_var ~loc (Ty_variable.of_input_var ~loc (quote_var s))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  Fold { idle_fold with ty_expr = pass_ty }


let reduction ~raise:_ = Iter.defaults

let decompile ~raise:_ =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_var v } when not (Ty_variable.is_generated v) ->
      (match String.chop_prefix ~prefix:"'" @@ Ty_variable.to_name_exn v with
      | Some unquoted -> t_arg ~loc unquoted
      | None -> make_t ~loc (T_var v))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  Fold { idle_fold with ty_expr = pass_ty }


open Unit_test_helpers.Ty_expr

let%expect_test _ =
  {| (T_arg x) |} |-> compile;
  [%expect {| (T_var 'x) |}]

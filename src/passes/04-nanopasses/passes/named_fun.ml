open Simple_utils.Trace
open Ast_unified
open Pass_type
open Errors

(*
  in Jsligo, parameters in arrow types have name, we simply drop it
  `(foo:int) => (bar:string) => nat` |-> `int -> string -> nat`

  note: we could remember them somehow in the future
  *)

include Flag.No_arg ()

let compile ~raise:_ =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_named_fun (params, ret) } ->
      let params = List.map ~f:(fun { type_expr; name = _ } -> type_expr) params in
      t_fun_of_list ~loc (params @ [ ret ])
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  Fold { idle_fold with ty_expr = pass_ty }


let reduction ~raise =
  { Iter.defaults with
    ty_expr =
      (function
      | { wrap_content = T_named_fun _; _ } -> raise.error (wrong_reduction __MODULE__)
      | _ -> ())
  }


let name = __MODULE__
let decompile ~raise:_ = Nothing

open Unit_test_helpers.Ty_expr

let%expect_test "compile" =
  {| (T_named_fun
        ((((name (foo)) (type_expr (TY_EXPR1))) ((name (bar)) (type_expr (TY_EXPR2))))
         (TY_EXPR3))) |}
  |-> compile;
  [%expect {| (T_fun ((TY_EXPR1) (T_fun ((TY_EXPR2) (TY_EXPR3))))) |}]

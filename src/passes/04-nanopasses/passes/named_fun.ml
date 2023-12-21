open Simple_utils.Trace
open Ast_unified
open Pass_type
open Errors

(*
  in Jsligo, parameters in arrow types have name, we simply drop it
  `(foo:int) => (bar:string) => nat` |-> `int -> string -> nat`
  *)

include Flag.No_arg ()

let compile ~raise:_ =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_named_fun (params, ret) } ->
      let param_names, params =
        List.fold_map
          ~init:[]
          ~f:(fun param_names { type_expr; name } -> name :: param_names, type_expr)
          params
      in
      inject_param_names (List.rev param_names) @@ t_fun_of_list ~loc (params @ [ ret ])
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

let decompile ~raise:_ =
  let pass_ty : _ ty_expr_ -> ty_expr = function
    | { location = loc; wrap_content = T_fun (param_names, param, rhs) } ->
      let len = List.length param_names in
      let rest_params, ret = extract_arguments_from_arrows (len - 1) rhs in
      let params = param :: rest_params in
      List.zip_opt param_names params
      |> Option.value_map
           ~default:(t_named_fun ~loc ([ { name = "_"; type_expr = param } ], rhs))
           ~f:(fun names_and_params ->
             t_named_fun
               ~loc
               ( List.map
                   ~f:(fun (name, type_expr) -> Named_fun.{ name; type_expr })
                   names_and_params
               , ret ))
    | { location = loc; wrap_content } -> make_t ~loc wrap_content
  in
  Fold { idle_fold with ty_expr = pass_ty }


open Unit_test_helpers.Ty_expr

let%expect_test "compile" =
  {| (T_named_fun
        ((((name foo) (type_expr (TY_EXPR1))) ((name bar) (type_expr (TY_EXPR2))))
         (TY_EXPR3))) |}
  |-> compile;
  [%expect {| (T_fun ((foo bar) (TY_EXPR1) (T_fun (() (TY_EXPR2) (TY_EXPR3))))) |}]

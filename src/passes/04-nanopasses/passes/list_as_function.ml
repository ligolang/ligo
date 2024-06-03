open Ast_unified
open Pass_type
open Errors
module Trace = Simple_utils.Trace
module Location = Simple_utils.Location
include Flag.No_arg ()

(* handle TS pattern to build lists from a function (cons, and literals) *)

let name = __MODULE__

let compile ~(raise : _ Trace.raise) =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_call (f, { wrap_content = args; location = _ }) ->
      (match get_e f with
      | E_variable v when Variable.is_name v "list" ->
        (match args with
        | [ arg ] ->
          (match get_e arg with
          | E_array elems -> e_array_as_list ~loc elems
          | _ -> raise.error @@ list_called_not_on_array same false)
        | [] -> raise.error @@ list_called_not_on_array same true
        | _ -> raise.error @@ list_called_not_on_array same false)
      | _ -> same)
    | _ -> same
  in
  Fold { idle_fold with expr = pass_expr }


let reduction ~(raise : _ Trace.raise) =
  let fail () = raise.error (wrong_reduction __MODULE__) in
  { Iter.defaults with
    expr =
      (function
      | { wrap_content = E_call (f, _); _ }
        when Option.value_map ~default:false (get_e_variable f) ~f:(fun x ->
                 Variable.is_name x "list") -> fail ()
      | _ -> ())
  }


let decompile ~raise:_ =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    let list_var = e_variable ~loc (Variable.of_input_var ~loc "list") in
    match Location.unwrap e with
    | E_constant { cons_name = C_CONS; arguments = [ hd; tl ] } ->
      let args =
        Location.wrap ~loc [ e_array ~loc Array_repr.[ Expr_entry hd; Rest_entry tl ] ]
      in
      e_call ~loc list_var args
    | E_list elements ->
      let args =
        Location.wrap
          ~loc
          [ e_array ~loc (List.map ~f:(fun x -> Array_repr.Expr_entry x) elements) ]
      in
      e_call ~loc list_var args
    | _ -> same
  in
  Fold { idle_fold with expr = pass_expr }


open Unit_test_helpers.Expr

let%expect_test _ =
  {|
    (E_call
      (E_variable list)
      ((E_array ((Expr_entry (EXPR1)) (Rest_entry (EXPR2))))))
  |}
  |-> compile;
  [%expect {|
      (E_array_as_list ((Expr_entry (EXPR1)) (Rest_entry (EXPR2))))
    |}]

let%expect_test _ =
  {|
    (E_call (E_variable list)
      ((E_array
        ((Expr_entry (EXPR1)) (Expr_entry (EXPR2)) (Expr_entry (EXPR3))))))
  |}
  |-> compile;
  [%expect
    {|
    (E_array_as_list
     ((Expr_entry (EXPR1)) (Expr_entry (EXPR2)) (Expr_entry (EXPR3)))) |}]

let%expect_test _ =
  {|
    (E_call (E_variable list)
      ((E_array
        ((Expr_entry (EXPR1)) (Rest_entry (EXPR2)) (Rest_entry (EXPR3))))))
  |}
  |-> compile;
  [%expect
    {|
    (E_array_as_list
     ((Expr_entry (EXPR1)) (Rest_entry (EXPR2)) (Rest_entry (EXPR3)))) |}]

let%expect_test _ =
  {| (E_constant ((cons_name C_CONS) (arguments ((EXPR1) (EXPR2))))) |} |-> decompile;
  [%expect
    {|
      (E_call (E_variable list)
       ((E_array ((Expr_entry (EXPR1)) (Rest_entry (EXPR2)))))) |}]

let%expect_test _ =
  {| (E_list ((EXPR1) (EXPR2) (EXPR3))) |} |-> decompile;
  [%expect
    {|
      (E_call (E_variable list)
       ((E_array ((Expr_entry (EXPR1)) (Expr_entry (EXPR2)) (Expr_entry (EXPR3))))))
    |}]

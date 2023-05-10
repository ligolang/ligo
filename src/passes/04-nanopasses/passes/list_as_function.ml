open Ast_unified
open Pass_type
open Simple_utils.Trace
open Errors
module Location = Simple_utils.Location

(* handle TS pattern to build lists from a function (cons, and literals) *)
let name = __MODULE__

include Flag.No_arg ()

let array_to_list ~raise ~loc (arguments : expr Array_repr.t) =
  match arguments with
  | [ Expr_entry hd; Rest_entry tl ] ->
    e_constant ~loc { cons_name = C_CONS; arguments = [ hd; tl ] }
  | [ Rest_entry ls; Rest_entry rs ] ->
    let p = Variable.fresh ~loc () in
    let l =
      e_record_access ~loc { struct_ = e_variable p ~loc; label = Label.of_int 0 }
    in
    let r =
      e_record_access ~loc { struct_ = e_variable p ~loc; label = Label.of_int 1 }
    in
    let result = e_constant ~loc { cons_name = C_CONS; arguments = [ l; r ] } in
    let lambda =
      Lambda.{ binder = Ligo_prim.Param.make p None; output_type = None; result }
    in
    let f = e_lambda ~loc lambda in
    e_constant ~loc { cons_name = C_LIST_FOLD_RIGHT; arguments = [ f; ls; rs ] }
  | _ ->
    let arguments =
      List.map arguments ~f:(function
          | Expr_entry x -> x
          | Rest_entry e -> raise.error (array_rest_not_supported e))
    in
    e_list ~loc arguments


let compile ~raise =
  let pass_expr : _ expr_ -> expr =
   fun e ->
    let loc = Location.get_location e in
    let same = make_e ~loc e.wrap_content in
    match Location.unwrap e with
    | E_call (f, { wrap_content = [ args ]; location = _ }) ->
      (match get_e f, get_e args with
      | E_variable v, E_array args when Variable.is_name v "list" ->
        array_to_list ~raise ~loc args
      | _ -> same)
    | _ -> same
  in
  Fold { idle_fold with expr = pass_expr }


let reduction ~raise =
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
  [%expect
    {|
      (E_constant ((cons_name C_CONS) (arguments ((EXPR1) (EXPR2)))))
    |}]

let%expect_test _ =
  {|
    (E_call (E_variable list)
      ((E_array
        ((Expr_entry (EXPR1)) (Expr_entry (EXPR2)) (Expr_entry (EXPR3))))))
  |}
  |-> compile;
  [%expect {| (E_list ((EXPR1) (EXPR2) (EXPR3))) |}]

let%expect_test _ =
  {|
    (E_call (E_variable list)
      ((E_array
        ((Expr_entry (EXPR1)) (Rest_entry (EXPR2)) (Rest_entry (EXPR3))))))
  |}
  |->! compile;
  [%expect {| Err : (Small_passes_array_rest_not_supported (E_variable #EXPR2)) |}]

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

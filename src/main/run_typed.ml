open Trace

let transpile_value
    (e:Ast_typed.annotated_expression) : Mini_c.value result =
  let%bind f =
    let open Transpiler in
    let (f , _) = functionalize e in
    let%bind main = translate_main f e.location in
    ok main
  in

  let input = Mini_c.Combinators.d_unit in
  let%bind r = Run_mini_c.run_entry f input in
  ok r

let evaluate_typed
    ?(debug_mini_c = false) ?(debug_michelson = false)
    ?options (entry:string) (program:Ast_typed.program) : Ast_typed.annotated_expression result =
  trace (simple_error "easy evaluate typed") @@
  let%bind result =
    let%bind mini_c_main =
      Transpiler.translate_entry program entry in
    (if debug_mini_c then
       Format.(printf "Mini_c : %a\n%!" Mini_c.PP.function_ mini_c_main)
    ) ;
    Run_mini_c.run_entry ?options ~debug_michelson mini_c_main (Mini_c.Combinators.d_unit)
  in
  let%bind typed_result =
    let%bind typed_main = Ast_typed.get_entry program entry in
    Transpiler.untranspile result typed_main.type_annotation in
  ok typed_result

let run_typed
    ?(debug_mini_c = false) ?(debug_michelson = false) ?options (entry:string)
    (program:Ast_typed.program) (input:Ast_typed.annotated_expression) : Ast_typed.annotated_expression result =
  let%bind () =
    let open Ast_typed in
    let%bind (Declaration_constant (d , _)) = get_declaration_by_name program entry in
    let%bind (arg_ty , _) =
      trace_strong (simple_error "entry-point doesn't have a function type") @@
      get_t_function @@ get_type_annotation d.annotated_expression in
    Ast_typed.assert_type_value_eq (arg_ty , (Ast_typed.get_type_annotation input))
  in

  let%bind mini_c_main =
    trace (simple_error "transpile mini_c entry") @@
    Transpiler.translate_entry program entry in
  (if debug_mini_c then
     Format.(printf "Mini_c : %a\n%!" Mini_c.PP.function_ mini_c_main)
  ) ;

  let%bind mini_c_value = transpile_value input in

  let%bind mini_c_result =
    let error =
      let title () = "run Mini_c" in
      let content () =
        Format.asprintf "\n%a" Mini_c.PP.function_ mini_c_main
      in
      error title content in
    trace error @@
    Run_mini_c.run_entry ~debug_michelson ?options mini_c_main mini_c_value in
  let%bind typed_result =
    let%bind main_result_type =
      let%bind typed_main = Ast_typed.get_functional_entry program entry in
      match (snd typed_main).type_value' with
      | T_function (_, result) -> ok result
      | _ -> simple_fail "main doesn't have fun type" in
    Transpiler.untranspile mini_c_result main_result_type in
  ok typed_result

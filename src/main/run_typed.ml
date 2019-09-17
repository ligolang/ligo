open Trace

let transpile_value
    (e:Ast_typed.annotated_expression) : Mini_c.value result =
  let%bind (f , ty) =
    let open Transpiler in
    let (f , _) = functionalize e in
    let%bind main = translate_main f e.location in
    ok main
  in

  let input = Mini_c.Combinators.d_unit in
  let%bind r = Run_mini_c.run_entry f ty input in
  ok r

let evaluate_typed
    ?(debug_mini_c = false) ?(debug_michelson = false)
    ?options (entry:string) (program:Ast_typed.program) : Ast_typed.annotated_expression result =
  trace (simple_error "easy evaluate typed") @@
  let%bind result =
    let%bind (mini_c_main , ty) =
      Transpiler.translate_entry program entry in
    (if debug_mini_c then
       Format.(printf "Mini_c : %a\n%!" Mini_c.PP.function_ mini_c_main)
    ) ;
    Run_mini_c.run_entry ?options ~debug_michelson mini_c_main ty (Mini_c.Combinators.d_unit)
  in
  let%bind typed_result =
    let%bind typed_main = Ast_typed.get_entry program entry in
    Transpiler.untranspile result typed_main.type_annotation in
  ok typed_result

(* returns a big_map if any. used to reconstruct the map from the diff when uncompiling *)
let rec fetch_big_map (v: Mini_c.value) : Mini_c.value option =
  match v with
  | D_pair (l , r) ->
    begin
    match (fetch_big_map l) with
    | Some _ as s -> s
    | None -> fetch_big_map r 
    end
  | D_big_map _ as bm -> Some bm 
  | _ -> None

(* try to convert expression to a literal *)
let rec exp_to_value (exp: Mini_c.expression) : Mini_c.value result =
  let open! Mini_c in
  match exp.content with
    | E_literal v -> ok @@ v
    | E_constant ("map" , lst) ->
      let aux el =
        let%bind l = exp_to_value el in
        match l with
          | D_pair (a , b) -> ok @@ (a , b)
          | _ -> fail @@ simple_error "??" in
      let%bind lstl = bind_map_list aux lst in
      ok @@ D_map lstl
    | E_constant ("big_map" , lst) ->
      let aux el =
        let%bind l = exp_to_value el in
        match l with
          | D_pair (a , b) -> ok @@ (a , b)
          | _ -> fail @@ simple_error "??" in
      let%bind lstl = bind_map_list aux lst in
      ok @@ D_big_map lstl
    | E_constant ("PAIR" , fst::snd::[]) ->
      let%bind fstl = exp_to_value fst in
      let%bind sndl = exp_to_value snd in
      ok @@ D_pair (fstl , sndl)
    | E_constant ("UNIT", _) -> ok @@ D_unit
    | E_constant ("UPDATE", _) ->
      let rec handle_prev upd =
        match upd.content with
        | E_constant ("UPDATE" , [k;v;prev]) ->
          begin
            match v.content with
              | E_constant ("SOME" , [i]) ->
                let%bind kl = exp_to_value k in
                let%bind il  = exp_to_value i in
                let%bind prevl = handle_prev prev in
                ok @@ (kl,il)::prevl
              | E_constant ("NONE" , []) ->
                let%bind prevl = handle_prev prev in
                ok @@ prevl
              | _ -> failwith "UPDATE second parameter is not an option"
          end
        | E_make_empty_map _ ->
          ok @@ []
        | _ -> failwith "impossible"
      in
      begin
      match exp.type_value with
        | T_big_map _ ->
          let%bind kvl = handle_prev exp in
          ok @@ D_big_map kvl
        | T_map _ ->
          let%bind kvl = handle_prev exp in
          ok @@ D_map kvl
        | _ -> failwith "UPDATE with a non-map type_value"
      end
    | _ ->
      fail @@ simple_error "Can not convert expression to literal"

let convert_to_literals (e:Ast_typed.annotated_expression) : Mini_c.value result =
  let open Transpiler in
  let%bind exp = translate_annotated_expression e in (*Mini_c.expression*)
  let%bind value = exp_to_value exp in
  ok @@ value

let run_typed
    ?(input_to_value = false) ?(debug_mini_c = false) ?(debug_michelson = false) ?options (entry:string)
    (program:Ast_typed.program) (input:Ast_typed.annotated_expression) : Ast_typed.annotated_expression result =
  let%bind () =
    let open Ast_typed in
    let%bind (Declaration_constant (d , _)) = get_declaration_by_name program entry in
    let%bind (arg_ty , _) =
      trace_strong (simple_error "entry-point doesn't have a function type") @@
      get_t_function @@ get_type_annotation d.annotated_expression in
    Ast_typed.assert_type_value_eq (arg_ty , (Ast_typed.get_type_annotation input))
  in

  let%bind (mini_c_main , ty) =
    trace (simple_error "transpile mini_c entry") @@
    Transpiler.translate_entry program entry in
  (if debug_mini_c then
     Format.(printf "Mini_c : %a\n%!" Mini_c.PP.function_ mini_c_main)
  ) ;

  let%bind mini_c_value = if input_to_value then 
    convert_to_literals input else transpile_value input in
  let bm_opt = if input_to_value then fetch_big_map mini_c_value else None in

  let%bind mini_c_result =
    let error =
      let title () = "run Mini_c" in
      let content () =
        Format.asprintf "\n%a" Mini_c.PP.function_ mini_c_main
      in
      error title content in
    trace error @@
    Run_mini_c.run_entry ~debug_michelson ?options ?bm_opt mini_c_main ty mini_c_value in
  let%bind typed_result =
    let%bind main_result_type =
      let%bind typed_main = Ast_typed.get_functional_entry program entry in
      match (snd typed_main).type_value' with
      | T_function (_, result) -> ok result
      | _ -> simple_fail "main doesn't have fun type" in
    Transpiler.untranspile mini_c_result main_result_type in
  ok typed_result

open Trace
open Function
module I = Multifix.Ast
module O = Ast_simplified

let unwrap : type a . a Location.wrap -> a = Location.unwrap

let type_variable : string -> O.type_expression result = fun str ->
  ok @@ O.T_variable str

let rec type_expression : I.type_expression -> O.type_expression result = fun te ->
  match te with
  | T_variable tv ->
      let%bind tv' = bind_map_location type_variable tv in
      ok @@ unwrap tv'
  | T_tuple lst ->
      let%bind lst' = bind_map_list (bind_map_location type_expression) lst in
      ok @@ O.T_tuple (List.map unwrap lst')
  | T_paren p ->
      let%bind p' = bind_map_location type_expression p in
      ok @@ unwrap p'
  | T_record r ->
      let aux : I.t_record_element -> _ = fun (T_record_element (s, te)) ->
        let%bind te' = bind_map_location type_expression te in
        ok (s, te')
      in
      let%bind r' = bind_map_list (bind_map_location aux) r in
      let te_map =
        let lst = List.map ((fun (x, y) -> unwrap x, unwrap y) >| unwrap) r' in
        let open Map.String in
        List.fold_left (fun prec (k , v) -> add k v prec) empty lst
      in
      ok @@ O.T_record te_map
  | T_application (f, arg) ->
      let%bind (f', arg') = bind_map_pair (bind_map_location type_expression) (f, arg) in
      let%bind name = match unwrap f' with
        | O.T_variable v -> ok v
        | _ -> simple_fail "can't apply to non-vars" in
      let args = match unwrap arg' with
        | T_tuple lst -> lst
        | x -> [ x ] in
      ok @@ O.T_constant (name, args)

let restricted_type_expression : I.restricted_type_expression -> O.type_expression result = fun rte ->
  match rte with
  | Tr_variable tv ->
      let%bind tv' = bind_map_location type_variable tv in
      ok @@ unwrap tv'
  | Tr_paren te -> type_expression (unwrap te)

let rec expression : I.expression -> O.annotated_expression result = fun e ->
  match e with
  | E_sequence _
  | E_let_in _
  | E_ifthen _
  | E_ifthenelse _
    -> simple_fail "not block expressions in local expressions yet"
  | E_fun _ -> simple_fail "no local functions yet"
  | E_match _ -> simple_fail "no match in expressions yet"
  | E_main m ->
      let%bind m' = bind_map_location expression_main m in
      ok @@ unwrap m'
  | E_record r ->
      let aux : I.e_record_element -> _ = fun re ->
        match re with
        | E_record_element_record_implicit _ -> simple_fail "no implicit record element yet"
        | E_record_element_record_explicit (s, e) ->
            let%bind e' = bind_map_location expression_no_seq e in
            ok (s, e')
      in
      let%bind r' = bind_map_list (bind_map_location aux) r in
      let e_map =
        let lst = List.map ((fun (x, y) -> unwrap x, unwrap y) >| unwrap) r' in
        let open Map.String in
        List.fold_left (fun prec (k , v) -> add k v prec) empty lst
      in
      ok @@ O.(ae @@ E_record e_map)

and expression_main : I.expression_main -> O.annotated_expression result = fun em ->
  let return x = ok O.(ae x) in
  let simple_binop name ab =
    let%bind (a' , b') = bind_map_pair (bind_map_location expression_main) ab in
    return @@ E_constant (name, [unwrap a' ; unwrap b']) in
  match em with
  | Eh_tuple lst ->
      let%bind lst' = bind_map_list (bind_map_location expression_main) lst in
      return @@ E_tuple (List.map unwrap lst')
  | Eh_application farg ->
      (* TODO: constructor case *)
      let%bind farg' = bind_map_pair (bind_map_location expression_main) farg in
      return @@ E_application (Tuple.map2 unwrap farg')
  | Eh_type_annotation (e, te) ->
      let%bind e' = bind_map_location expression_main e in
      let%bind e'' = match (unwrap e').type_annotation with
        | None -> ok (unwrap e').expression
        | Some _ -> simple_fail "can't double annotate" in
      let%bind te' = bind_map_location restricted_type_expression te in
      ok @@ O.annotated_expression e'' (Some (unwrap te'))
  | Eh_lt ab ->
      simple_binop "LT" ab
  | Eh_gt ab ->
      simple_binop "GT" ab
  | Eh_le ab ->
      simple_binop "LE" ab
  | Eh_eq ab ->
      simple_binop "EQ" ab
  | Eh_cons ab ->
      simple_binop "CONS" ab
  | Eh_addition ab ->
      simple_binop "ADD" ab
  | Eh_substraction ab ->
      simple_binop "MINUS" ab
  | Eh_multiplication ab ->
      simple_binop "TIMES" ab
  | Eh_division ab ->
      simple_binop "DIV" ab
  | Eh_int n ->
      return @@ E_literal (Literal_int (unwrap n))
  | Eh_string s ->
      return @@ E_literal (Literal_string (unwrap s))
  | Eh_unit _ ->
      return @@ E_literal Literal_unit
  | Eh_tz _ ->
      simple_fail "tz literals not supported yet"
  | Eh_module_ident _ ->
      simple_fail "modules not supported yet"
  | Eh_variable v ->
      return @@ E_variable (unwrap v)
  | Eh_constructor _ ->
      simple_fail "constructor without parameter"
  | Eh_list _ ->
      simple_fail "list not supported yet"
  | Eh_name _ ->
      simple_fail "named parameter not supported yet"
  | Eh_assign _ ->
      simple_fail "assign not supported yet"
  | Eh_accessor _ ->
      simple_fail "accessor not supported yet"
  | Eh_bottom e ->
      expression (unwrap e)

and expression_no_seq : I.expression_no_seq -> O.annotated_expression result = fun mns ->
  match mns with
  | _ -> simple_fail "todo"

let statement : I.statement -> O.declaration result = fun s ->
  match s with
  | Statement_variable_declaration ([n], e) ->
      let%bind (name, ty) =
        let%bind pattern =
          match unwrap n with
          | Param_restricted_pattern c -> ok c
          | Param_implicit_named_param _ -> simple_fail "" in
        match unwrap pattern with
        | Pr_restrict c -> (
            match unwrap c with
            | P_type_annotation (l, te) -> (
                let%bind v = match unwrap l with
                  | P_variable v -> ok v
                  | _ -> simple_fail "no sugar-candy for regular declarations yet"
                in
                ok (v, te)
              )
            | _ -> simple_fail "no sugar-candy for regular declarations yet"
          )
        | Pr_variable _ -> simple_fail "provide type for top-level declarations!"
        | Pr_unit _ -> simple_fail "define unit is meaningless"
      in
      let name' = unwrap name in
      let%bind e' = bind_map_location expression e in
      let%bind ty' = bind_map_location restricted_type_expression ty in
      let%bind e'' = match (unwrap e').type_annotation with
        | None -> ok (unwrap e').expression
        | Some _ -> simple_fail "can't add an annotation at the expression of a declaration" in
      let ae = O.annotated_expression e'' (Some (unwrap ty')) in
      ok @@ O.Declaration_constant {name = name' ; annotated_expression = ae}
  | Statement_variable_declaration _ -> simple_fail "no sugar-candy for fun declarations yet"
  | Statement_init_declaration _ -> simple_fail "no init declaration yet"
  | Statement_entry_declaration _ -> simple_fail "no entry declaration yet"
  | Statement_type_declaration (n, te) ->
      let%bind te' = bind_map_location type_expression te in
      ok @@ O.Declaration_type {type_name = unwrap n ; type_expression = unwrap te'}

let program : I.program -> O.program result = fun (Program lst) ->
  bind_map_list (apply Location.unwrap >| bind_map_location statement) lst

let main : I.entry_point -> O.program Location.wrap result =
  bind_map_location program

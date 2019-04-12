open Trace
open Function
module I = Multifix.Ast
module O = Ast_simplified

let unwrap = Location.unwrap

let expression : I.expression -> O.expression result = fun _ ->
  simple_fail ""

let type_variable : string -> O.type_expression result = fun str ->
  ok @@ O.T_variable str

let type_expression : I.type_expression -> O.type_expression result = fun te ->
  match te with
  | T_variable tv ->
      let%bind tv' = bind_map_location type_variable tv in
      ok @@ unwrap tv'
  | _ -> simple_fail "lel"

let restricted_type_expression : I.restricted_type_expression -> O.type_expression result = fun rte ->
  match rte with
  | Tr_variable tv ->
      let%bind tv' = bind_map_location type_variable tv in
      ok @@ unwrap tv'
  | Tr_paren te -> type_expression (unwrap te)

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
      let ae = O.annotated_expression (unwrap e') (Some (unwrap ty')) in
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

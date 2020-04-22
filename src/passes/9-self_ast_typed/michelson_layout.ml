open Ast_typed
open Trace

let get_label_map_from_env (v:expression_variable) (env: full_environment) : expression_label_map result = 
  let%bind a = trace_option (simple_error "corner case") @@
    Environment.get_opt v env in
  ( match a.definition with
    | ED_declaration { expr = {expression_content = E_record lmap_e;_} ; _} -> ok lmap_e
    | _ -> simple_fail "corner case" )

let rec to_right_comb_e l new_map =
  match l with
  | [] -> new_map 
  | [ (_, expl) ; (_ , expr) ] ->
    LMap.add_bindings [ (Label "0" , expl) ; (Label "1" , expr) ] new_map
  | (_, exp)::tl ->
    let new_map' = LMap.add (Label "0") exp new_map in
    LMap.add (Label "1") ({exp with expression_content = E_record (to_right_comb_e tl new_map')}) new_map'

let rec to_left_comb_e_ first l new_map =
  match l with
  | [] -> new_map 
  | (_, expl) :: (_, expr) ::tl when first ->
    let new_map' = LMap.add_bindings [ (Label "0" , expl) ; (Label "1" , expr) ] LMap.empty in
    to_left_comb_e_ false tl new_map'
  | (_,exp)::tl ->
    let new_map' = LMap.add_bindings [
      (Label "0" , {exp with expression_content = E_record new_map});
      (Label "1" , exp ) ;] LMap.empty in
    to_left_comb_e_ first tl new_map'

let to_left_comb_e = to_left_comb_e_ true

let to_sorted_kv_list (l_e:expression_label_map) (l_t:te_lmap) : (label * expression) list =
  let l = List.combine (LMap.to_kv_list l_e) (LMap.to_kv_list l_t) in
  let sorted' = List.sort
    (fun (_,(_,{decl_position=a;_})) (_,(_,{decl_position=b;_})) -> Int.compare a b) l in
  List.map (fun (e,_t) -> e) sorted'

let peephole_expression : expression -> expression result = fun e ->
  let return expression_content = ok { e with expression_content } in
  match e.expression_content with
  | E_constant {cons_name= (C_CONVERT_TO_RIGHT_COMB | C_CONVERT_TO_LEFT_COMB ) as converter;
                arguments= [ {
                  expression_content = record_exp;
                  type_expression = {type_content = T_record lmap_t} }
                ] } ->

    let%bind lmap_e = match record_exp with
      | E_record lmap_e -> ok lmap_e
      | E_variable v -> get_label_map_from_env v e.environment
      | _ -> simple_fail "corner case" in

    let kvl = to_sorted_kv_list lmap_e lmap_t in 
    let converted_exp = match converter with
      | C_CONVERT_TO_RIGHT_COMB -> E_record (to_right_comb_e kvl LMap.empty)
      | C_CONVERT_TO_LEFT_COMB -> E_record (to_left_comb_e kvl LMap.empty)
      | _ -> e.expression_content
    in

    return converted_exp
  | _ as e -> return e
module Errors = Errors
module Helpers = Helpers

let all_module_passes ~raise ~warn_unused_rec = [
  Unused.unused_map_module ~raise;
  Muchused.muchused_map_module ~raise;
  Helpers.map_module @@ Recursion.check_tail_expression ~raise ;
  Helpers.map_module @@ Recursion.remove_rec_expression ~raise ~warn_unused_rec ;
  Helpers.map_module @@ Pattern_matching_simpl.peephole_expression ;
]

let all_expression_passes ~raise ~warn_unused_rec = [
  Helpers.map_expression @@ Recursion.check_tail_expression ~raise ;
  Helpers.map_expression @@ Recursion.remove_rec_expression ~raise ~warn_unused_rec ;
  Pattern_matching_simpl.peephole_expression ;
]

let contract_passes ~raise = [
  (* REMITODO: Move old self_mini_c.ml "self in lambda" check *)
  Contract_passes.self_typing ~raise ;
  No_nested_big_map.self_typing ~raise ;
  Contract_passes.entrypoint_typing ~raise ;
]

let all_module ~raise ~warn_unused_rec init =
  List.fold ~f:(|>) (all_module_passes ~raise ~warn_unused_rec) ~init

let all_expression ~raise ~warn_unused_rec init =
  List.fold ~f:(|>) (all_expression_passes ~raise ~warn_unused_rec) ~init

let all_contract ~raise main_name prg =
  let contract_type = Helpers.fetch_contract_type ~raise main_name prg in
  let data : Contract_passes.contract_pass_data = {
    contract_type = contract_type ;
    main_name = main_name ;
    } in
  let all_p = List.map ~f:(fun pass -> Ast_typed.Helpers.fold_map_module pass data) @@ contract_passes ~raise in
  let prg = List.fold ~f:(fun x f -> snd @@ f x) all_p ~init:prg in
  let prg = Contract_passes.remove_unused ~raise data prg in
  prg

let all_view ~raise views_name main_name prg =
  let f view_name =
    let view_type,view_loc = Helpers.fetch_view_type ~raise view_name prg in
    let contract_type = Helpers.fetch_contract_type ~raise main_name prg in
    View_passes.check_view_type ~raise ~err_data:(view_loc,main_name,view_name) contract_type view_type
  in
  let () = List.iter ~f views_name in
  prg

let all = [
  Recursion.check_tail_expression
]

let remove_unused_expression = Contract_passes.remove_unused_expression

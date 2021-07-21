open Trace
module Errors = Errors
module Helpers = Helpers

let all_module_passes ~add_warning = [
  Unused.unused_map_module ~add_warning;
  Muchused.muchused_map_module ~add_warning;
  Helpers.map_module Tail_recursion.peephole_expression ;
  Helpers.map_module Michelson_layout.peephole_expression ;
  Helpers.map_module Pattern_matching_simpl.peephole_expression ;
]

let all_expression_passes = [
  Helpers.map_expression Tail_recursion.peephole_expression ;
  Helpers.map_expression Michelson_layout.peephole_expression ;
  Pattern_matching_simpl.peephole_expression ;
]

let contract_passes = [
  Contract_passes.self_typing ;
  No_nested_big_map.self_typing ;
]

let all_module ~add_warning =
  bind_chain (all_module_passes ~add_warning)

let all_expression =
  bind_chain all_expression_passes

let all_contract main_name prg =
  let* contract_type = Helpers.fetch_contract_type main_name prg in
  let data : Contract_passes.contract_pass_data = {
    contract_type = contract_type ;
    main_name = main_name ;
    } in
  let all_p = List.map ~f:(fun pass -> Helpers.fold_map_module pass data) contract_passes in
  bind_chain_ignore_acc all_p prg
let all = [
  Tail_recursion.peephole_expression
]

let map_expression = Helpers.map_expression

let fold_expression = Helpers.fold_expression

let fold_map_expression = Helpers.fold_map_expression

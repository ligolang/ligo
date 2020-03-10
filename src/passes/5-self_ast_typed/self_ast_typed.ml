open Trace

let all_passes = []

let contract_passes = [
  Contract_passes.self_typing ;
]

let all_program =
  let all_p = List.map Helpers.map_program all_passes in
  bind_chain all_p

let all_expression =
  let all_p = List.map Helpers.map_expression all_passes in
  bind_chain all_p

let all_contract main_name prg =
  let%bind contract_type = Helpers.fetch_contract_type main_name prg in
  let data : Contract_passes.contract_pass_data = {
    contract_type = contract_type ;
    main_name = main_name ;
    } in
  let all_p = List.map (fun pass -> Helpers.fold_map_program pass data) contract_passes in
  bind_chain_ignore_acc all_p prg

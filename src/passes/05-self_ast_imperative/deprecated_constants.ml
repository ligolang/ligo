open Ast_imperative

let warn_constant l curr alt ty =
  `Self_ast_imperative_warning_deprecated_constant (l, curr, alt, ty)

let get_alternative t element =
  if ValueVar.(equal element @@ of_input_var "balance") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_balance", t_arrow (t_unit ()) (t_tez ()))
  else if ValueVar.(equal element @@ of_input_var "amount") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_amount", t_arrow (t_unit ()) (t_tez ()))
  else if ValueVar.(equal element @@ of_input_var "now") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_now", t_arrow (t_unit ()) (t_timestamp ()))
  else if ValueVar.(equal element @@ of_input_var "sender") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_sender", t_arrow (t_unit ()) (t_address ()))
  else if ValueVar.(equal element @@ of_input_var "source") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_source", t_arrow (t_unit ()) (t_address ()))
  else if ValueVar.(equal element @@ of_input_var "level") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_level", t_arrow (t_unit ()) (t_nat ()))
  else if ValueVar.(equal element @@ of_input_var "self_address") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_self_address", t_arrow (t_unit ()) (t_address ()))
  else if ValueVar.(equal element @@ of_input_var "chain_id") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_chain_id", t_arrow (t_unit ()) (t_chain_id ()))
  else if ValueVar.(equal element @@ of_input_var "total_voting_power") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_total_voting_power", t_arrow (t_unit ()) (t_nat ()))
  else if ValueVar.(equal element @@ of_input_var "min_block_time") && ModuleVar.(equal t @@ of_input_var "Tezos") then
    Some (t, ValueVar.of_input_var "get_min_block_time", t_arrow (t_unit ()) (t_nat ()))
  else
    None

let warn ~raise : expression -> expression = fun e ->
  let () = match e.expression_content with
    | E_module_accessor { module_path = [t] ; element }
         when Option.is_some @@ get_alternative t element ->
       let t, n, ty = Option.value_exn (get_alternative t element) in
       raise.Simple_utils.Trace.warning (warn_constant e.location e (e_module_accessor [t] n) ty)
    | _ -> () in
  e

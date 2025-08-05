type storage = (address, tez) big_map
type return_type = operation list * storage

[@entry]
let withdraw (tx_amount : tez) (storage : storage) : return_type =
  (* Verify that the caller has enough balance for the withdrawal *)
  let old_balance = Big_map.find (Tezos.get_sender ()) storage in
  let _ = if tx_amount > old_balance then failwith "Insufficient balance" in
  (* Create transaction *)
  let receiver_account = match Tezos.get_contract_opt (Tezos.get_sender ()) with
    Some account -> account
  | None -> failwith "Couldn't find account" in
  let operation = Tezos.Operation.transaction unit tx_amount receiver_account in
  (* Update balance *)
  let new_balance : tez = Option.value_with_error "Unreachable error; we already compared balance to amount" (old_balance - tx_amount) in
  let new_storage = Big_map.update (Tezos.get_sender ()) (Some new_balance) storage in
  [operation], new_storage
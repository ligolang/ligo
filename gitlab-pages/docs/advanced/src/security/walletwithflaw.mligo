module WalletWithFlaw = struct

  (* Variant for two types of transactions *)
  type transaction =
    Deposit of address * tez
  | Withdrawal of address * tez

  type storage = {
    owner : address;
    transactionLog : transaction list
  }

  type return_type = operation list * storage

  (* Receive a deposit *)
  [@entry]
  let deposit (_ : unit) (storage : storage) : return_type =
    (* Verify that tez was sent *)
    let _ = if Tezos.get_amount () = 0tez then failwith "Send tez to deposit" in
    (* Add log entry *)
    let newLogEntry : transaction = Deposit (Tezos.get_sender (), Tezos.get_amount ()) in
    [], { storage with transactionLog = newLogEntry :: storage.transactionLog }

  (* Return a withdrawal *)
  [@entry]
  let withdraw (tx_destination, tx_amount : address * tez) (storage : storage) : return_type =
    (* Verify that the sender is the admin *)
    let _ = if Tezos.get_sender () <> storage.owner then failwith "Not the owner" in
    (* Verify that no tez was sent *)
    let _ = if Tezos.get_amount () <> 0tez then failwith "Don't send tez to this entrypoint" in
    (* Create transaction *)
    let callee = Tezos.get_contract_opt tx_destination in
    let operation = match callee with
      Some contract ->
        Tezos.Operation.transaction () tx_amount contract
    | None -> failwith "Couldn't send withdrawal to that address"
    in
    (* Add log entry and return operation and new log *)
    let newLogEntry : transaction = Withdrawal (tx_destination, tx_amount) in
    [operation], { storage with transactionLog = newLogEntry :: storage.transactionLog }

end
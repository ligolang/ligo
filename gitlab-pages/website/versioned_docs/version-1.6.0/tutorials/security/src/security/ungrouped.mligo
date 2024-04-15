type parameter = Fund | Send of address * tez

type transaction = Incoming of address * tez | Outgoing of address * tez

type storage = {owner : address; transactionLog : transaction list}

type result = operation list * storage

let do_send (dst, @amount : address * tez) =
  let callee = Tezos.get_contract_opt dst in
  match callee with
    Some contract ->
      let op = Tezos.transaction () @amount contract in
      Outgoing (dst, @amount), [op]
  | None -> (failwith "Could not send tokens" : transaction * operation list)

let do_fund (from, @amount : address * tez) =
  Incoming (from, @amount), ([] : operation list)

[@entry]
let fund (_ : unit) (s : storage) : result =
  let tx, ops = do_fund (Tezos.get_sender (), Tezos.get_amount ()) in
  ops, { s with transactionLog = tx :: s.transactionLog }

[@entry]
let send (args : address * tez) (s : storage) =
  let u = assert ((Tezos.get_sender ()) = s.owner && (Tezos.get_amount ()) = 0mutez) in
  let tx, ops = do_send args in
  ops, { s with transactionLog = tx :: s.transactionLog }
type storage = {beneficiary : address; balances : (address, tez) map}

type parameter = tez * (unit contract)

let withdraw (param, s : parameter * storage) =
  let @amount, beneficiary = param in
  let beneficiary_addr = Tezos.address beneficiary in
  let @balance =
    match (Map.find_opt beneficiary_addr s.balances) with
      Some v -> v
    | None -> 0mutez in
  let new_balance = match @balance - @amount with
    | Some x -> x
    | None -> (failwith "Insufficient balance" : tez)
  in
  let op = Tezos.transaction () @amount beneficiary in
  let new_balances =
    Map.update beneficiary_addr (Some new_balance) s.balances in
  [op], {s with balances = new_balances}
type storage = {owner : address; beneficiaries : address list}

let send_rewards (beneficiary_addr : address) =
  let maybe_contract =
    Tezos.get_contract_opt beneficiary_addr in
  let beneficiary =
    match maybe_contract with
      Some contract -> contract
    | None -> (failwith "CONTRACT_NOT_FOUND" : unit contract) in
  Tezos.transaction () 5000000mutez beneficiary

let main (p, s : unit * storage) =
  if (Tezos.get_sender ()) <> s.owner
  then (failwith "ACCESS_DENIED" : operation list * storage)
  else
    let ops = List.map send_rewards s.beneficiaries in
    ops, s
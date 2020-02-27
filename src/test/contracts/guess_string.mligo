type storage = {
  challenge : string
}

type param = {
  new_challenge : string;
  attempt       : string
}

type return = operation list * storage

let attempt (p: param; store : storage) : return =
  (* if p.attempt <> store.challenge then failwith "Failed challenge" else *)
  let contract : unit contract = Operation.get_contract sender in
  let transfer : operation =
    Operation.transaction (unit, contract, 10.00tez) in
  let store : storage = {challenge = p.new_challenge}
  in ([] : operation list), store

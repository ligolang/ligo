type storage = {
  challenge : string;
}

type param = {
  new_challenge : string;
  attempt       : bytes;
}

let attempt (p: param) storage =
  if Crypto.hash (Bytes.pack p.attempt) <> Bytes.pack storage.challenge
  then failwith "Failed challenge"
  else
    let contract : unit contract =
      Operation.get_contract sender in
    let transfer : operation =
      Operation.transaction (unit, contract, 10tz) in
    let storage : storage = {challenge = p.new_challenge}
    in ([] : operation list), storage

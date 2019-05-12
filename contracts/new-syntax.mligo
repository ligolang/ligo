(** Type of storage for this contract *)
type storage = {
  challenge : string ;
}

(** Initial storage *)
let%init storage = {
  challenge = "" ;
}

type param = {
  new_challenge : string ;
  attempt : bytes ;
}

let%entry attempt (p:param) storage =
  if Crypto.hash (Bytes.pack p.attempt) <> Bytes.pack storage.challenge then failwith "Failed challenge" ;
  let contract : unit contract = Operation.get_contract sender in
  let transfer : operation = Operation.transaction (unit , contract , 10.00tz) in
  let storage : storage = storage.challenge <- p.new_challenge in
  ((list [] : operation list), storage)

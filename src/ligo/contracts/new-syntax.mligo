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
  let transfer : operation = Operation.transfer (sender , 10tz) in
  let storage : storage = storage.challenge <- p.new_challenge in
  ((list [] : operation list), storage)

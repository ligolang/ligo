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
  attempt : string ;
}

let%entry attempt (p:param) storage =
  (* if p.attempt <> storage.challenge then failwith "Failed challenge" else *)
  let contract : unit contract = Operation.get_contract sender in
  let transfer : operation = Operation.transaction (unit , contract , 10.00tz) in
  (* TODO: no syntax for functional updates yet *)
  (* let storage : storage = { storage with challenge = p.new_challenge } in *)
  (* for now, rebuild the record by hand. *)
  let storage : storage = { challenge = p.new_challenge } in
  ((list [] : operation list), storage)

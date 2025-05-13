type parameter = Back | Claim | Withdraw

type storage = {
  owner    : address;
  goal     : tez;
  deadline : timestamp;
  backers  : (address, tez) map;
  funded   : bool
}

[@entry]
let back (param : unit) (store : storage) : operation list * storage = (* Annotation *)
  if Tezos.get_now () > store.deadline then failwith "Deadline passed."
  else
    match Map.find_opt (Tezos.get_sender ()) store.backers with
      None ->
        let backers = Map.update (Tezos.get_sender ()) (Some (Tezos.get_amount ())) store.backers
        in [], {store with backers=backers}
    | Some (x) -> [], store
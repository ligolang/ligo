let main ((s, _) : string * unit) : operation list * unit =
  let u = match (Tezos.call_view s (Tezos.get_sender ()) ("tz1fakefakefakefakefakefakefakcphLA5" : address) : unit option) with
    | Some x -> x
    | None -> () in
  ([] : operation list), u

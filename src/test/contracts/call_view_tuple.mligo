let main ((_, _) : unit * unit) : operation list * unit =
  let u = match (Tezos.call_view "foo" (Tezos.get_sender (), 1, "foo", 1n) ("tz1fakefakefakefakefakefakefakcphLA5" : address) : unit option) with
    | Some x -> x
    | None -> () in
  ([] : operation list), u

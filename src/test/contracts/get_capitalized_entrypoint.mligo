let main (_ : unit * unit) : operation list * unit =
  let dst : (unit contract) option = Tezos.get_entrypoint_opt "%Upper" (Tezos.get_sender ()) in
  match dst with
  | None -> failwith "lol"
  | Some dst ->
    let op : operation = Tezos.transaction () 0mutez dst in
    ([op], ())

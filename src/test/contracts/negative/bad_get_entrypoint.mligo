[@entry]
let main (_ : unit) (_ : unit) : operation list * unit =
  let v : unit contract option =
    Tezos.get_entrypoint_opt
      "foo"
      ("tz1fakefakefakefakefakefakefakcphLA5" : address) in
  let u : unit =
    match v with
      None -> failwith "None"
    | Some _ -> failwith "Some"
  in [], u

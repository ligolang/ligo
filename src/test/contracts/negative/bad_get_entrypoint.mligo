[@entry]
let main (_ : unit) (_ : unit) : operation list * unit =
  let v =
    (Tezos.get_entrypoint_opt
       "foo"
       ("tz1fakefakefakefakefakefakefakcphLA5" : address)
     : unit contract option) in
  let u : unit =
    match v with
      None -> failwith "None"
    | Some _ -> failwith "Some" in
  ([] : operation list), u

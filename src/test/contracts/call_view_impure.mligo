[@entry]
let main () () : operation list * unit =
  let u =
    match (Tezos.View.call
         "foo"
         (Tezos.get_sender ())
         ("tz1fakefakefakefakefakefakefakcphLA5" : address)
       : unit option)
    with
      Some x -> x
    | None -> () in
  [], u

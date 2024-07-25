module Test = Test.Next

let originate_record () =
    let orig = Test.Originate.from_file "./unit_contract.mligo" () 0tez in
    let addr = Test.Typed_address.to_address orig.taddr in
    let contr = Test.Typed_address.to_contract orig.taddr in
    {
        contr = contr ;
        addr  = addr  ;
        taddr = (orig.taddr :(unit, unit) typed_address) ;
    }

let test =
    let r = originate_record () in
    let packed = Bytes.pack (fun() ->
        match (Tezos.get_entrypoint_opt "%transfer" r.addr : unit contract option) with
          Some(c) -> let op = Tezos.Next.Operation.transaction () 0mutez c in [op]
        | None ->  ([] : operation list)
    ) in
    let () = Test.IO.log(packed) in
    ()

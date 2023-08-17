let originate_record () =
    let storage = Test.compile_value () in
    let (addr, _, _) = Test.originate_from_file "./unit_contract.mligo" "main" storage 0tez in
    let taddr : (unit, unit) typed_address = Test.cast_address addr in
    let contr = Test.to_contract taddr in
    {
        contr = contr ;
        addr  = addr  ;
        taddr = taddr ;
    }

let test =
    let r = originate_record () in
    let packed = Bytes.pack (fun() -> 
        match (Tezos.get_entrypoint_opt "%transfer" r.addr : unit contract option) with
          Some(c) -> let op = Tezos.transaction () 0mutez c in [op]
        | None ->  ([] : operation list)
    ) in
    let () = Test.log(packed) in
    ()
module C = struct
  [@entry]
  let main (_ : unit) (_ : unit) : operation list * unit =
      let (op, _) = Tezos.create_contract (fun ((k,v) : int * nat) (s : (int, nat) big_map) : operation list * (int, nat) big_map ->
          ([] : operation list), Big_map.add k v s) (None : key_hash option) 0tez (Big_map.empty : (int, nat) big_map) in
      ([ op ; ], ())
end

let test_main =
    let orig =  Test.originate (contract_of C) () 0tez in
    let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
    ()

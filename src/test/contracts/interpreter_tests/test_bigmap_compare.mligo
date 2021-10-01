let main (a, s : address * ((address * nat), nat) big_map) : operation list * ((address * nat), nat) big_map =
  ([] : operation list), Big_map.add (a, 1n) 1234n s

let test = 
    let () = Test.reset_state 10n ([] : tez list) in
    let a1 = Test.nth_bootstrap_account 1 in
    let initial_storage = Big_map.literal [((a1, 0n), 42n)] in
    let (taddr, _,_) = Test.originate main initial_storage 0tez in
    let contr = Test.to_contract taddr in
    let () = Test.set_source a1 in
    let () = Test.transfer_to_contract_exn contr a1 1mutez in 
    let ns = Test.get_storage taddr in
    assert (Big_map.find_opt (a1, 1n) ns = Some 1234n)

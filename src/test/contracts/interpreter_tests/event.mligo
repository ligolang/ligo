let main (p,_ : (int*int) * unit ) : operation list * unit =
  [Tezos.emit "%foo" p ; Tezos.emit "%foo" p.0],()

let test_foo =
  let (ta, _, _) = Test.originate main () 0tez in
  let _ = Test.transfer_to_contract_exn (Test.to_contract ta) (1,2) 0tez in
  (Test.get_last_events_from ta "foo" : (int*int) list),(Test.get_last_events_from ta "foo" : int list)
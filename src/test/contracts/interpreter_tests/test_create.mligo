let main_factory (i : int) (s : address list) : operation list * address list =
  let main (d : int) (k : int) : operation list * int = ([], d + k) in
  let (op, addr) = Tezos.create_contract main (None : key_hash option) 1tz i in
  [op], addr :: s

let test =
  let (fact_ta, _, _) = Test.originate main_factory ([] : address list) 10tez in
  let fact_contract = Test.to_contract fact_ta in
  let _ = Test.transfer_to_contract_exn fact_contract 42 0tez in
  let addr : address = Option.unopt (List.head_opt (Test.get_storage fact_ta)) in
  let _ : (int, int) typed_address = Test.cast_address addr in
  let taddr : (int, int) typed_address = Test.cast_address addr in
  let () = Test.log (Test.get_storage taddr) in
  Test.log (Test.get_storage_of_address addr)

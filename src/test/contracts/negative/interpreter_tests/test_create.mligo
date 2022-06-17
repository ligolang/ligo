let main_factory ((i, s) : int * address list) : operation list * address list =
  let main ((d, k) : int * int) : operation list * int = ([], d + k) in
  let (op, addr) = Tezos.create_contract main (None : key_hash option) 1tz i in
  [op], addr :: s

let test =
  let (fact_ta, _, _) = Test.originate main_factory ([] : address list) 10tez in
  let fact_contract = Test.to_contract fact_ta in
  let _ = Test.transfer_to_contract_exn fact_contract 42 0tez in
  let addr : address = Option.unopt (List.head_opt (Test.get_storage fact_ta)) in
  Test.log (Test.get_storage_of_address addr)

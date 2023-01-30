let main (_ : unit) (_ : unit) : operation list * unit =
  let _v = (failwith "foo" : unit) in
  ([] : operation list), ()

let make_call (contr : unit contract) =
  let _ = Test.get_storage_of_address ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
  Test.transfer_to_contract_exn contr () 10tez


let test =
  let (ta, _, _) = Test.originate main () 1tez in
  make_call (Test.to_contract ta)

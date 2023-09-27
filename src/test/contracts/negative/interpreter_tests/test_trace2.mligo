module C = struct
  [@entry]
  let main () () : operation list * unit = failwith "foo"
end

let make_call (contr : C parameter_of contract) =
  let () = Test.get_storage_of_address ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
  Test.transfer_to_contract_exn contr (Main ()) 10tez


let test =
  let orig = Test.originate (contract_of C) () 1tez in
  make_call (Test.to_contract orig.addr)

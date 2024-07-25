module Test = Test.Next

module C = struct
  [@entry]
  let main () () : operation list * unit = failwith "foo"
end

let make_call (contr : C parameter_of contract) =
  let () = Test.Address.get_storage ("KT1RYW6Zm24t3rSquhw1djfcgQeH9gBdsmiL" : address) in
  Test.Contract.transfer_exn contr (Main ()) 10tez


let test =
  let orig = Test.Originate.contract (contract_of C) () 1tez in
  make_call (Test.Typed_address.to_contract orig.taddr)

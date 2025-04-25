module Test = Test.Next
module Tezos = Tezos.Next

module C = Contract_under_test.Fail_contract

let test =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  let contr = Test.Typed_address.to_contract orig.taddr in
  let addr = Tezos.address contr in
  match Test.Contract.transfer contr (Main ()) 10tez with
  | Success _ -> (failwith "Should fail !" : michelson_program )
  | Fail e -> (
    match e with
    | Rejected x ->
      let (x, addr_fail) = x in
      let () = Assert.assert (addr_fail = addr) in
      x
    | _ -> (failwith "Failed, but wrong reason" : michelson_program )
  )

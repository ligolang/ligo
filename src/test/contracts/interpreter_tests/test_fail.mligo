#import "./contract_under_test/fail_contract.mligo" "C"

module Test = Test.Next

let assert = Assert.assert

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
      let () = assert (addr_fail = addr) in
      x
    | _ -> (failwith "Failed, but wrong reason" : michelson_program )
  )

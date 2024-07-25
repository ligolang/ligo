#include "./contract_under_test/fail_contract.mligo"
let under_test = "./contract_under_test/fail_contract.mligo"

module Test = Test.Next
let assert = Assert.assert

let test =
  let _vfail = Test.Michelson.run (fun () -> fail_data) () in
  let orig = Test.Originate.from_file under_test () 0tez in
  match Test.Typed_address.transfer orig.taddr () 10tez with
  | Success _ -> (failwith "Should fail !" : michelson_program )
  | Fail e -> (
    match e with
    | Rejected (x, addr_fail) ->
      let () = assert (addr_fail = Test.Typed_address.to_address orig.taddr) in
      x
    | _ -> (failwith "Failed, but wrong reason" : michelson_program )
  )

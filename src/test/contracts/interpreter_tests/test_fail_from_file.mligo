#include "./contract_under_test/fail_contract.mligo"
let under_test = "./contract_under_test/fail_contract.mligo"

let test =
  let _vfail = Test.run (fun () -> fail_data) () in
  let orig = Test.originate_from_file under_test () 0tez in
  match Test.transfer orig.addr () 10tez with
  | Success _ -> (failwith "Should fail !" : michelson_program )
  | Fail e -> (
    match e with
    | Rejected (x, addr_fail) ->
      let () = assert (addr_fail = Test.to_address orig.addr) in
      x
    | _ -> (failwith "Failed, but wrong reason" : michelson_program )
  )

#include "./contract_under_test/fail_contract.mligo"
let under_test = "./contract_under_test/fail_contract.mligo"

let test =
  let vunit = Test.compile_value () in
  let _vfail = Test.run (fun () -> fail_data) () in
  let (addr,_code,_) = Test.originate_from_file under_test "main" ([] : string list) vunit 0tez in

  match Test.transfer addr vunit 10tez with
  | Success _ -> (failwith "Should fail !" : michelson_program )
  | Fail e -> (
    match e with
    | Rejected x ->
      let (x, addr_fail) = x in
      let () = assert (addr_fail = addr) in
      x
    | _ -> (failwith "Failed, but wrong reason" : michelson_program )
  )

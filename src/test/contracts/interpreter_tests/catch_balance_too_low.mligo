#include "./contract_under_test/contract_create.mligo"


let test =
  let baker = Test.nth_bootstrap_account 0 in
  let src = Test.nth_bootstrap_account 1 in

  let (typed_addr, code, size) = Test.originate main (None : storage) 0tez in

  let contr = Test.to_contract typed_addr in
  match Test.transfer_to_contract contr Two 1mutez with
  | Fail (Balance_too_low {contract_too_low ; contract_balance ; spend_request}) ->
    let () = assert (contract_balance =  1mutez) in
    let () = assert (spend_request = 1tez) in
    ()
  | _ ->
    failwith "should have failed because the balance was too low"
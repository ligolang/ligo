#include "./contract_under_test/contract_create.mligo"


let test =

  let (typed_addr, _code, _size) = Test.originate main (None : storage) 0tez in

  let contr = Test.to_contract typed_addr in
  match Test.transfer_to_contract contr Two 1mutez with
  (* TODO this is a bug :( *)
  | Fail (Balance_too_low {contract_too_low = _ ; contract_balance ; spend_request}) ->
    let () = assert (contract_balance =  1mutez) in
    let () = assert (spend_request = 1tez) in
    ()
  | _ ->
    failwith "should have failed because the balance was too low"

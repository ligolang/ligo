#import "./contract_under_test/contract_create.mligo" "C"

let test =
  let orig = Test.originate (contract_of C) None 0tez in
  let contr = Test.to_contract orig.addr in
  match Test.transfer_to_contract contr (Main Two) 1mutez with
  (* TODO this is a bug :( *)
  | Fail (Balance_too_low {contract_too_low = _ ; contract_balance ; spend_request}) ->
    let () = assert (contract_balance =  1mutez) in
    let () = assert (spend_request = 1tez) in
    ()
  | _ ->
    failwith "should have failed because the balance was too low"

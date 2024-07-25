#import "./contract_under_test/contract_create.mligo" "C"

let assert = Assert.assert
module Test = Test.Next

let test =
  let orig = Test.Originate.contract (contract_of C) None 0tez in
  let contr = Test.Typed_address.to_contract orig.taddr in
  match Test.Contract.transfer contr (Main Two) 1mutez with
  (* TODO this is a bug :( *)
  | Fail (Balance_too_low {contract_too_low = _ ; contract_balance ; spend_request}) ->
    let () = assert (contract_balance =  1mutez) in
    let () = assert (spend_request = 1tez) in
    ()
  | _ ->
    failwith "should have failed because the balance was too low"

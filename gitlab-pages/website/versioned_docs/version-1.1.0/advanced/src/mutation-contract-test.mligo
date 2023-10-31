(* This is mutation-contract-test.mligo *)

#import "mutation-contract.mligo" "MutationContract"

type storage = int
type param = MutationContract parameter_of
let initial_storage = 7

let tester (taddr : (param, storage) typed_address) (_ : michelson_contract) (_ : int) : unit =
  let contr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn contr (Add 7) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 7)

let test_original =
  let (taddr, _, _) = Test.originate_module (contract_of MutationContract) initial_storage 0tez in
  tester taddr

let test_mutation =
  match Test.originate_module_and_mutate (contract_of MutationContract) initial_storage 0tez tester with
    None -> ()
  | Some (_, mutation) ->
    let () = Test.log(mutation) in
    failwith "A mutation of the contract still passes the tests!"

let tester_add_and_sub (taddr : (param, storage) typed_address) (_ : michelson_contract) (_ : int) : unit =
  let contr = Test.to_contract taddr in
  let _ = Test.transfer_to_contract_exn contr (Add 7) 1mutez in
  let () = assert (Test.get_storage taddr = initial_storage + 7) in
  let _ = Test.transfer_to_contract_exn contr (Sub 3) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 4)
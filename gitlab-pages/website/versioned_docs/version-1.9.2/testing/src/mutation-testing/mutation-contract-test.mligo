(* This is mutation-contract-test.mligo *)

#import "gitlab-pages/docs/testing/src/mutation-testing/mutation-contract.mligo" "MutationContract"
module Test = Test.Next
type storage = MutationContract.AddSub.storage
type param = MutationContract.AddSub parameter_of
let initial_storage = 7

let tester (taddr : (param, storage) typed_address) (_ : (param ,storage) michelson_contract) (_:int) : unit =
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "add" taddr) 7 0tez in
  Assert.assert (Test.Typed_address.get_storage taddr = initial_storage + 7)

let test_original =
  let orig = Test.Originate.contract (contract_of MutationContract.AddSub) initial_storage 0tez in
  tester orig.taddr
let test_mutation =
  match Test.Mutation.contract (contract_of MutationContract.AddSub) initial_storage 0tez tester with
    None -> ()
  | Some (_, mutation) ->
    let () = Test.IO.log(mutation) in
    (* In a real program, one would write `failwith "A mutation passes"`
       Since we want to demonstrate the issue without an actual error
       a milder println is used in this document. *)
    Test.IO.println "A mutation of the contract still passes the tests!"
let tester_add_and_sub (taddr : (param, storage) typed_address) (_ : (param, storage) michelson_contract) (_ : int) : unit =
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "add" taddr) 7 0tez in
  let () = Assert.assert (Test.get_storage taddr = initial_storage + 7) in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "sub" taddr) 3 0tez in
  Assert.assert (Test.get_storage taddr = initial_storage + 4)

let test_mutation_sub =
  match Test.Mutation.contract (contract_of MutationContract.AddSub) initial_storage 0tez tester_add_and_sub with
    None -> ()
  | Some (_, mutation) ->
    let () = Test.IO.log(mutation) in
    Test.IO.println "A mutation of the contract still passes the tests!"
let test_mutation_all =
  match Test.Mutation.All.contract (contract_of MutationContract.AddSub) initial_storage 0tez tester with
    [] -> ()
  | ms ->
      let () = Test.IO.println "Some mutation also passes the tests! ^^" in
      List.iter (fun ((_, mutation) : unit * mutation) -> Test.IO.log mutation) ms
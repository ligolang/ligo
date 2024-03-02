(* This is mutation-contract-test.mligo *)

#import "gitlab-pages/docs/advanced/src/mutation-testing/mutation-contract.mligo" "MutationContract"

type storage = MutationContract.C.storage
type param = MutationContract.C parameter_of
let initial_storage = 7

let tester (taddr : (param, storage) typed_address) (_: (param ,storage) michelson_contract) (_:int) : unit =
  let _ = Test.transfer_exn taddr (Add 7) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 7)

let test_original =
  let orig = Test.originate (contract_of MutationContract.C) initial_storage 0tez in
  tester orig.addr
let test_mutation =
  match Test.originate_module_and_mutate (contract_of MutationContract.C) initial_storage 0tez tester with
    None -> ()
  | Some (_, mutation) ->
    let () = Test.log(mutation) in
    (* In a real program, one would write `failwith "A mutation passes"`
       Since we want to demonstrate the issue without an actual error
       a milder println is used in this document. *)
    Test.println "A mutation of the contract still passes the tests!"
let tester_add_and_sub (taddr : (param, storage) typed_address) (_ : (param, storage) michelson_contract) (_ : int) : unit =
  let _ = Test.transfer_exn taddr (Add 7) 1mutez in
  let () = assert (Test.get_storage taddr = initial_storage + 7) in
  let _ = Test.transfer_exn taddr (Sub 3) 1mutez in
  assert (Test.get_storage taddr = initial_storage + 4)
let test_mutation_all =
  match Test.originate_and_mutate_all (contract_of MutationContract.C) initial_storage 0tez tester_add_and_sub with
    [] -> ()
  | ms -> let () = List.iter (fun ((_, mutation) : unit * mutation) ->
                              let path = Test.save_mutation "." mutation in
                              let () = Test.log "saved at:" in
                              Test.log path) ms in
          Test.println "Some mutations also pass the tests!"
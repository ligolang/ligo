#import "./contract_under_test/contract_create.mligo" "C"

module Test = Test.Next
let assert = Test.Assert.assert


let check_new_origination (src :address) : address =
  let last_origs = Test.State.last_originations () in
  match Map.find_opt src last_origs with
    | Some new_lst -> (
      let () = assert (List.length new_lst = 1n) in
      match new_lst with
      | new_acc::_rst -> new_acc
      | [] -> (failwith "more than one originated account" : address)
    )
    | None -> (failwith "source did not originate anything" : address)

let test =
  let _baker = Test.Account.address 0 in
  let src = Test.Account.address 1 in

  let {taddr = typed_addr; code = _; size} = Test.Originate.contract (contract_of C) None 0tez in
  let () = assert ((None : C.storage) = (Test.Typed_address.get_storage typed_addr)) in
  let () = assert (size < 300) in
  let new_account1 = check_new_origination src in

  let _ = Test.Typed_address.transfer_exn typed_addr (Main Two) 10tez in
  let new_account2 = check_new_origination new_account1 in
  let new_storage = Test.Typed_address.get_storage typed_addr in
  let expected_new_storage = Some new_account2 in
  let () = assert (new_storage = expected_new_storage) in

  match (Test.Typed_address.transfer typed_addr (Main One) 10tez : test_exec_result) with
  | Success _ -> (failwith "contract did not fail" : michelson_program)
  | Fail x -> (
    let x = (fun (x : test_exec_error) -> x) x in
    match x with
    | Rejected reject_data ->
      let (v,addr) = reject_data in
      let () = assert (addr = new_account2) in
      let () = assert (addr = new_account2) in
      let () = assert (Test.Compare.eq v (Test.Michelson.eval 111)) in
      v
    | _ -> (failwith "contract failed for another reason" : michelson_program)
  )

let test2 =
  // By default:
  //  - only 2 bootstrap accounts are created with a default amount of 4000000000000 mutez
  //  - the 1st and 2nd bootstrap accounts are used as baker and source respectively

  // You can change the default behavior by reseting the state:
  let number_of_account = 4n in
  let overide_default_amounts = [ 8000tez ; 2mutez ] in // the [i]th element of the list overwrite default balance of the [i]th account
  let () = Test.State.reset number_of_account overide_default_amounts in
  // And by setting the source in between calls to `Test.transfer_to_contract` or `Test.Originate.contract`
  let bsa0 = (Test.Account.address 0) in
  let bsa1 = (Test.Account.address 1) in
  let bsa2 = (Test.Account.address 2) in
  let bsa3 = (Test.Account.address 3) in
  let () = Test.State.set_source bsa3 in
  let () = Test.State.set_baker bsa2 in
  // some balance tests:
  let tz = fun (n:nat) ->
    Test.Michelson.run (fun (x : unit -> nat) -> x () * 1mutez) (fun (_ : unit) -> n)
  in
  let () = assert ((Test.Address.get_balance bsa0) = 2000tez) in
  let () = assert ((Test.Address.get_balance bsa1) = 0mutez) in
  let () = assert (Test.Compare.eq (Test.Michelson.eval (Test.Address.get_balance bsa1)) (tz 0n)) in
  let () = assert ((Test.Address.get_balance bsa2) = 3800000tez) in
  let () = assert ((Test.Address.get_balance bsa3) = 3800000000000mutez) in
  ()

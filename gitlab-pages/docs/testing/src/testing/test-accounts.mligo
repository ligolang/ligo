module Counter = struct
  type storage = int * address
  type return_type = operation list * storage

  [@entry] let increment (n : int) (storage : storage) : return_type =
    let (number, admin_account) = storage in
    [], (number + n, admin_account)

  [@entry] let sub (n : int) (storage : storage) : return_type =
    let (number, admin_account) = storage in
    [], (number - n, admin_account)

  [@entry] let reset (_ : unit) (storage : storage) : return_type =
    let (_number, admin_account) = storage in
    if Tezos.get_sender() = admin_account then
      [], (0, admin_account)
    else
      failwith "Only the owner can call this entrypoint"
end
module Test = Test.Next

let test_admin =
  let (admin_account, user_account) = (Test.Account.address(0n), Test.Account.address(1n)) in

  // Originate the contract with the admin account in storage
  let initial_storage = (10, admin_account) in
  let orig = Test.Originate.contract (contract_of Counter) initial_storage 0tez in

  // Try to call the reset entrypoint as the user and expect it to fail
  let () = Test.State.set_source user_account in
  let result = Test.Contract.transfer (Test.Typed_address.get_entrypoint "reset" orig.taddr ) unit 0tez in
  let () = match result with
    Fail _err -> Test.IO.log "Test succeeded"
  | Success _s -> failwith "User should not be able to call reset" in

  // Call the reset entrypoint as the admin and expect it to succeed
  let () = Test.State.set_source admin_account in
  let _: nat = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "reset" orig.taddr) unit 0tez in

  let (newNumber, _admin_account) = Test.Typed_address.get_storage orig.taddr in
  Assert.assert (newNumber = 0)
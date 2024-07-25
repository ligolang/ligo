module Test = Test.Next

module Timelock = Test.Timelock

let assert = Test.Assert.assert

module C = struct
  type storage = bytes
  type parameter = chest_key * chest

  type return = operation list * storage

  [@entry] let check (p : parameter) (_s : storage) : return =
    let (ck,c) = p in
    let new_s =
      match Tezos.open_chest ck c 10n with
      | Some b -> b
      | _ -> 0xff
    in [], new_s
end


let test =
  let init_storage : bytes = 0x41414141 in
  let orig = Test.Originate.contract (contract_of C) init_storage 0tez in
  let payload = 0x4141
  in
  let test_open (cc : chest_key * chest) (expected : bytes) : unit =
    let x : C parameter_of contract = Test.Typed_address.to_contract orig.taddr in
    let _ = Test.Contract.transfer_exn x (Check cc) 0tez in
    let s = Test.Typed_address.get_storage orig.taddr in
    let _ = Test.IO.log (s, expected) in
    assert (s = expected)
  in
  let test1 = (* chest key/payload and time matches -> OK *)
    let chest, chest_key = Timelock.create payload 10n in
    test_open (chest_key, chest) payload
  in
  let test2 = (* chest key/payload do not match *)
    let chest, _ = Timelock.create payload 10n in
    let _, chest_key = Timelock.create 0x2020 10n in
    test_open (chest_key,chest) 0xff
  in
  let test3 = (* chest time does not match *)
    let chest, _ = Timelock.create payload 2n in
    let chest_key = Timelock.create_key chest 10n in
    test_open (chest_key, chest) 0x
  in
  let test4 = (* verify *)
    let chest1, chest_key1 = Timelock.create payload 2n in
    let _chest2, chest_key2 = Timelock.create payload 2n in
    let () = assert (not (Timelock.verify chest1 chest_key2 2n)) in
    assert (Timelock.verify chest1 chest_key1 2n)
  in [test1; test2; test3; test4]

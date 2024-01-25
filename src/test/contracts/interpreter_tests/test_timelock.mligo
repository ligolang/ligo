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
  let orig = Test.originate (contract_of C) init_storage 0tez in
  let payload = 0x4141
  in
  let test_open (cc : chest_key * chest) (expected : bytes) : unit =
    let x : C parameter_of contract = Test.to_contract orig.addr in
    let _ = Test.transfer_to_contract_exn x (Check cc) 0tez in
    let s = Test.get_storage orig.addr in
    let _ = Test.log (s, expected) in
    assert (s = expected)
  in
  let test1 = (* chest key/payload and time matches -> OK *)
    let chest, chest_key = Test.create_chest payload 10n in
    test_open (chest_key, chest) payload
  in
  let test2 = (* chest key/payload do not match *)
    let chest, _ = Test.create_chest payload 10n in
    let _, chest_key = Test.create_chest 0x2020 10n in
    test_open (chest_key,chest) 0xff
  in
  let test3 = (* chest time does not match *)
    let chest, _ = Test.create_chest payload 2n in
    let chest_key = Test.create_chest_key chest 10n in
    test_open (chest_key, chest) 0x
  in [test1; test2; test3]

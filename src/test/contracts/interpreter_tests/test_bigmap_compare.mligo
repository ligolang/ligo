let assert = Assert.assert

module Test = Test.Next

module C = struct
  [@entry]
  let main ((a, n) : address * nat) (s : ((address * nat), nat) big_map) : operation list * ((address * nat), nat) big_map =
    ([] : operation list), Big_map.add (a, 1n) n s
end

let test =
    let () = Test.State.reset 10n ([] : tez list) in
    let a1 = Test.Account.address 1 in
    let initial_storage = Big_map.literal [((a1, 0n), 42n)] in
    let orig = Test.Originate.contract (contract_of C) initial_storage 0tez in
    let () = Test.State.set_source a1 in
    let _ = Test.Typed_address.transfer_exn orig.taddr (Main (a1, 1234n)) 1mutez in
    let ns = Test.Typed_address.get_storage orig.taddr in
    let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 1n), 1234n)]) in
    let () = assert (Big_map.find_opt (a1, 1n) ns = Some 1234n) in
    let _ = Test.Typed_address.transfer_exn orig.taddr (Main (a1, 4321n)) 1mutez in
    let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 1n), 1234n)]) in
    let ns = Test.Typed_address.get_storage orig.taddr in
    let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 1n), 4321n)]) in
    let () = assert (ns = Big_map.literal [((a1, 0n), 42n); ((a1, 0n), 42n); ((a1, 1n), 4321n)]) in
    ()

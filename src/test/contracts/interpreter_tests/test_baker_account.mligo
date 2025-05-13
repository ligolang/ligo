module Test = Test.Next

module C = struct
  [@entry]
  let main (p : int) (s : int) : operation list * int = ([] : operation list), p + s
end

let test =
  let acc = Test.Account.new () in
  let () = Test.State.Reset.add_baker (acc.sk, acc.pk) (None : tez option) in
  let () = Test.State.reset 2n ([] : tez list) in
  let pkh = Crypto.hash_key acc.pk in
  let c = Tezos.implicit_account pkh in
  let a = Tezos.address c in
  let () = Test.IO.log "STARTING BALANCE AND VOTING POWER" in
  let () = Test.IO.log(Test.Address.get_balance a) in
  let () = Test.IO.log(Test.State.get_voting_power pkh) in
  let () = Test.State.set_baker a in
  let orig = Test.Originate.contract (contract_of C) 41 5tez in
  let () = Test.IO.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
  let () = Test.IO.log(Test.Address.get_balance a) in
  let () = Test.IO.log(Test.State.get_voting_power pkh) in
  let cc = Test.Typed_address.to_contract orig.taddr in
  let _ = Test.Contract.transfer cc (Main 1) 3tez in
  let () = Test.IO.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
  let () = Test.IO.log(Test.Address.get_balance a) in
  let () = Test.IO.log(Test.State.get_voting_power pkh) in
  ()

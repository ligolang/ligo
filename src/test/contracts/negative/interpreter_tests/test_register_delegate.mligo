module Test = Test.Next

let log = Test.IO.log

module C = struct
  [@entry]
  let main (p : int) (s : int) : operation list * int =
    ([] : operation list), p + s
end

let test =

  let acc = Test.Account.new () in
  let pkh = Crypto.hash_key acc.pk in
  let c = Tezos.implicit_account pkh in
  let a = Tezos.address c in

  let _ = Test.Contract.transfer_exn c () 100000tez in
  let () = Test.State.register_delegate pkh in
  let () = Test.State.bake_until 2n in

  let () = log "STARTING BALANCE AND VOTING POWER" in
  let () = log(Test.Address.get_balance a) in
  let () = log(Test.State.get_voting_power pkh) in
  let () = Test.State.set_baker a in
  let {taddr=ta; code=_; size=_} =
    Test.Originate.contract (contract_of C) 41 5tez in

  let () = log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
  let () = log(Test.Address.get_balance a) in
  let () = log(Test.State.get_voting_power pkh) in
  let cc = Test.Typed_address.to_contract ta in
  let _ = Test.Contract.transfer cc (Main 1) 3tez in

  let () = log "BALANCE AND VOTING POWER AFTER TRANSFER" in
  let () = log(Test.Address.get_balance a) in
  let () = log(Test.State.get_voting_power pkh) in
  ()

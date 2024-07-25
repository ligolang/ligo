module Test = Test.Next

module C = struct
  [@entry] let main (p : int) (s : int) : operation list * int = ([] : operation list), p + s
end

let test =

  let {addr=_; pk; sk=_} = Test.Account.new () in
  let pkh = Crypto.hash_key pk in
  let c = Tezos.implicit_account pkh in
  let a = Tezos.address c in

  let _ = Test.Contract.transfer_exn c () 1000000tez in
  let () = Test.State.register_delegate pkh in

  // TODO: Why does 8n fail, but 4n work?
  let () = Test.State.bake_until 4n in

  let () = Test.IO.log "STARTING BALANCE AND VOTING POWER" in
  let () = Test.IO.log(Test.Address.get_balance a) in
  let () = Test.IO.log(Test.State.get_voting_power pkh) in
  let () = Test.State.set_baker a in
  let orig = Test.Originate.contract (contract_of C) 41 5tez in

  let () = Test.IO.log "BALANCE AND VOTING POWER AFTER ORIGINATE" in
  let () = Test.IO.log(Test.Address.get_balance a) in
  let () = Test.IO.log(Test.State.get_voting_power pkh) in
  let _ = Test.Typed_address.transfer orig.taddr (Main 1) 3tez in

  let () = Test.IO.log "BALANCE AND VOTING POWER AFTER TRANSFER" in
  let () = Test.IO.log(Test.Address.get_balance a) in
  let () = Test.IO.log(Test.State.get_voting_power pkh) in
  ()

module Test = Test.Next
module Tezos = Tezos.Next

module C = struct
  [@entry] let main (p : int) (s : int) : operation list * int = ([] : operation list), p + s
end

let test =
  let acc = Test.Account.new () in
  let pkh = Crypto.hash_key acc.pk in
  let c = Tezos.implicit_account pkh in

  let _ = Test.Contract.transfer_exn c () 1000000tez in
  let () = Test.State.register_delegate pkh in
  let () = Test.State.stake pkh 1000000tez in
  ()

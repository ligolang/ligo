module C = struct
  [@entry] let main (p : int) (s : int) : operation list * int = ([] : operation list), p + s
end

let test =
  let acc = Test.new_account () in
  let pkh = Crypto.hash_key acc.1 in
  let c = Tezos.implicit_account pkh in

  let _ = Test.transfer_to_contract_exn c () 1000000tez in
  let () = Test.register_delegate pkh in
  let () = Test.stake pkh 1000000tez in
  ()

module Test = Test.Next

module C = struct
  [@entry]
  let main () () : operation list * unit = [], ()
end

let test =
  let {taddr; code=_; size=_} =
    Test.Originate.contract (contract_of C) () 0tez in
  let contr = Test.Typed_address.to_contract taddr in
  let addr = Tezos.address contr in
  let () = Test.IO.log addr in
  let () = Test.State.set_source addr in
  let _ = Test.Originate.contract (contract_of C) () 0tez in
  ()

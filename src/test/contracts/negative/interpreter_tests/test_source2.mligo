module Test = Test.Next

module C = struct
  [@entry]
  let main () () : operation list * unit = [], ()
end

let test =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  let () = Test.IO.log orig.taddr in
  let () = Test.State.set_source
             (Test.Typed_address.to_address orig.taddr) in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Main ()) 0tez in
  ()

module C = struct [@entry] let main () () : operation list * unit = [], () end

let test =
  let orig = Test.originate (contract_of C) () 0tez in
  let () = Test.log orig.addr in
  let () = Test.set_source (Test.to_address orig.addr) in
  let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
  ()

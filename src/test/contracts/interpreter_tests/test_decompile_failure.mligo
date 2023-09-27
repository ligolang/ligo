module C = struct
  [@entry]
  let main (() : unit) (() : unit) : operation list * unit =
    failwith (4n, ("a", 5))
end

let test =
  let orig = Test.originate (contract_of C) () 0tez in
  match Test.transfer orig.addr (Main ()) 0tez with
  | Success _ -> Test.log "OK"
  | Fail (Rejected (actual, _)) ->
    let ec : nat * (string * int) = Test.decompile actual in
    Test.log ec
  | Fail _ -> Test.log "KO"

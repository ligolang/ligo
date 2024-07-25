module Test = Test.Next

module C = struct
  [@entry]
  let main (() : unit) (() : unit) : operation list * unit =
    failwith (4n, ("a", 5))
end

let test =
  let orig = Test.Originate.contract (contract_of C) () 0tez in
  match Test.Typed_address.transfer orig.taddr (Main ()) 0tez with
  | Success _ -> Test.IO.log "OK"
  | Fail (Rejected (actual, _)) ->
    let ec : nat * (string * int) = Test.Michelson.decompile actual in
    Test.IO.log ec
  | Fail _ -> Test.IO.log "KO"

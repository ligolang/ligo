let main (() : unit) (() : unit) : operation list * unit =
  failwith (4n, ("a", 5))

let test =
  let taddr, _, _ = Test.originate main () 0tez in
  let c = Test.to_contract taddr in
  match Test.transfer_to_contract c () 0tez with
  | Success _ -> Test.log "OK"
  | Fail (Rejected (actual, _)) ->
    let ec : nat * (string * int) = Test.decompile actual in
    Test.log ec
  | Fail _ -> Test.log "KO"

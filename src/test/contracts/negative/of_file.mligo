let m () =
  [%michelson
  ({| { PUSH unit Unit ; PUSH mutez 300000000 ; NONE key_hash ; CREATE_CONTRACT (codestr $0) ; PAIR } |}
     [%of_file "./removed.tz"]
   : operation * address)]

[@entry]
let main (_ : unit) (_ : unit) : operation list * unit =
  let op, _ = m () in
  [op], ()

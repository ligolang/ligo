module Tezos = Tezos.Next

module Other_main = struct
  [@entry]
  let main (p : key_hash) (_ : unit) : operation list * unit =
    let _ : unit contract = Tezos.implicit_account p
    in [], ()
end

[@entry]
let main (p : key_hash) (s : unit) = Other_main.main p s

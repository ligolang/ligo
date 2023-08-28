module Other_main = struct
  [@entry]
  let main (p : key_hash) (s : unit) =
    let c : unit contract = Tezos.implicit_account p in
    ([] : operation list), unit

end

[@entry]
let main (p : key_hash) (s : unit) = Other_main.main p s

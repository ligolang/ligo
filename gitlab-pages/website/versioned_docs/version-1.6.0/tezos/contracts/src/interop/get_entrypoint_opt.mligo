type storage = int

type parameter = int

type x = Left of int

[@entry]
let main (p : parameter) (s : storage): operation list * storage =
  let contract =
    match Tezos.get_entrypoint_opt "%left" ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address) with
    | Some c -> c
    | None -> failwith "contract does not match"
  in
  [Tezos.transaction (Left 2) 2mutez contract], s
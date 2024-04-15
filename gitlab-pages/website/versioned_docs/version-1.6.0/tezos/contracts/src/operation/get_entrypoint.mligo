type storage = int
type parameter = int

type remote_param = Sub of int

[@entry]
let main (_ : parameter) (s : storage): operation list * storage =
  let contract_addr =
    Tezos.get_entrypoint
      "%sub" // Corresponds to the `Sub` variant of `remote_param`.
      ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)
  in [Tezos.transaction (Sub 2) 2mutez contract_addr], s
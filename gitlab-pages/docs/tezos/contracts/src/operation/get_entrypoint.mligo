type storage = int
type parameter = int

type remote_param = Sub of int

[@entry]
let main (_ : parameter) (s : storage): operation list * storage =
  let contract_addr =
    Tezos.get_entrypoint
      "%sub" // Corresponds to the `Sub` variant of `remote_param`.
      ("KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" : address)
  in [Tezos.transaction (Sub 2) 2mutez contract_addr], s
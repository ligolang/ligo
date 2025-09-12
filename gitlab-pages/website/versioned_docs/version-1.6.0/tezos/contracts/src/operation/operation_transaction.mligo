type 'storage return = operation list * 'storage

module A =
  struct
    type storage = int

    [@entry]
    let add (delta : int) (storage : storage) : storage return =
      [], storage + delta
  end

module B =
  struct
    type storage = int

    [@entry]
    let increment (_param : unit) (storage : storage) : storage return =
      let contract_addr =
        Tezos.get_contract
          ("KT1TxqZ8QtKvLu3V3JH7Gx58n7Co8pgtpQU5" : address) in
      let operation =
        Tezos.transaction (Add 1 : A parameter_of) 0tez contract_addr
    in [operation], storage
  end
type returnType = operation list * int

type contractParam =
Reset
| Decrement of int
| Increment of int

[@entry]
let callContract (_ : unit) (storage : int) : returnType =
  let contractAddress : address = ("KT1FpuaoBHwXMXJ6zn3F4ZhpjpPZV28MAinz" : address) in
  let myContract: contractParam contract = Tezos.get_contract contractAddress in
  let operation = Tezos.transaction (Increment 4) 0tez myContract in
  [operation], storage
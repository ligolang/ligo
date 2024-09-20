module Test = Test.Next

type 'storage return = operation list * 'storage

module A = struct
    type storage = int

    [@entry]
    let add (delta : int) (storage : storage) : storage return =
      [], storage + delta

    [@entry]
    let sub (delta : int) (storage : storage) : storage return =
      [], storage - delta
  end

module B = struct
    type storage = address

    [@entry]
    let increment (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_contract stored_address in
      let parameter : A parameter_of = Add value in
      let operation = Tezos.Next.Operation.transaction parameter 0tez contract in
    [operation], stored_address

    [@entry]
    let decrement (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_contract stored_address in
      let parameter : A parameter_of = Sub value in
      let operation = Tezos.Next.Operation.transaction parameter 0tez contract in
    [operation], stored_address
  end

let test =
  // Originate contract A
  let contract_A = Test.Originate.contract (contract_of A) 0 0tez in
  let contract_A_address = Test.Typed_address.to_address contract_A.taddr in

  // Originate contract B with the address of contract A in its storage
  let contract_B = Test.Originate.contract (contract_of B) contract_A_address 0tez in

  // Call contract B
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "increment" contract_B.taddr) 10 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "decrement" contract_B.taddr) 2 0tez in

  let newNumber = Test.Typed_address.get_storage contract_A.taddr in
  Assert.assert (newNumber = 8)
module C = struct
    type storage = address

    [@entry]
    let increment (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_entrypoint "%add" stored_address in
      let operation = Tezos.Next.Operation.transaction value 0tez contract in
    [operation], stored_address

    [@entry]
    let decrement (value : int) (stored_address : storage) : storage return =
      let contract = Tezos.Next.get_entrypoint "%sub" stored_address in
      let operation = Tezos.Next.Operation.transaction value 0tez contract in
    [operation], stored_address
  end
module D = struct
  type storage = address
  type contract_a_param = (int, "sub", int, "add") michelson_or

  [@entry]
  let increment (value : int) (stored_address : storage) : storage return =
    let pass_to_add : contract_a_param = M_right value in
    let contract = Tezos.Next.get_contract stored_address in
    let operation = Tezos.Next.Operation.transaction pass_to_add 0tez contract in
  [operation], stored_address

  [@entry]
  let decrement (value : int) (stored_address : storage) : storage return =
    let pass_to_sub : contract_a_param = M_left value in
    let contract = Tezos.Next.get_contract stored_address in
    let operation = Tezos.Next.Operation.transaction pass_to_sub 0tez contract in
  [operation], stored_address
end
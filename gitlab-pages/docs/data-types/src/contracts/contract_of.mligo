type storage = int
type return = operation list * storage

module C = struct
  [@entry]
  let decrement (param : int) (storage : storage) : return =
    [], storage   - param

  [@entry]
  let increment (param : int) (storage : storage) : return =
    [], storage + param

  [@entry]
  let reset () (_ : storage) : return = [], 0
end

let test_initial_storage () : unit =
  let init_storage = 42 in
  let fee = 0mutez in
  let contract = Test.Originate.contract (contract_of C) init_storage fee in

  (* Call contract through entrypoints *)
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "increment" contract.taddr) 15 0tez in
  let _ = Test.Contract.transfer_exn (Test.Typed_address.get_entrypoint "decrement" contract.taddr) 14 0tez in

  (* Call contract through `main` function *)
  let increment_param : C parameter_of = Increment 8 in
  let decrement_param : C parameter_of = Decrement 3 in
  let _ = Test.Typed_address.transfer_exn contract.taddr increment_param 0mutez in
  let _ = Test.Typed_address.transfer_exn contract.taddr decrement_param 0mutez in

  let new_storage = Test.Typed_address.get_storage contract.taddr
  in Assert.assert (new_storage = init_storage + 15 - 14 + 8 - 3)
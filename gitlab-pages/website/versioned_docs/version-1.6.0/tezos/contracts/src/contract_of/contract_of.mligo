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
  let orig = Test.Next.originate (contract_of C) init_storage fee in
  let new_storage = Test.Next.Typed_address.get_storage orig.taddr
  in assert (new_storage = init_storage)
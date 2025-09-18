type storage = int
type return = operation list * storage

module Foo = struct
  [@entry]
  let decrement (param : int) (storage : storage) : return =
    [], storage - param

  [@entry]
  let increment (param : int) (storage : storage) : return =
    [], storage + param

  [@entry]
  let reset () (_ : storage) : return = [], 0

  [@view]
  let get_storage () (storage : storage) : storage = storage
end
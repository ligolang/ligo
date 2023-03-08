module C = struct
  type storage = int

  [@entry]
  let add (delta : int) (store : storage) : operation list * storage = [], store + delta
  [@entry]
  let sub (delta : int) (store : storage) : operation list * storage = [], store - delta
  [@entry]
  let reset () (_ : storage) : operation list * storage = [], 0
end

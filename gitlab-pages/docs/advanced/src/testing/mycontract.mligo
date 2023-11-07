// This is mycontract.mligo
module C = struct
  type storage = int
  type result = operation list * storage

  // Two entrypoints
  [@entry] let increment (delta : int) (store : storage) : result = [],store + delta
  [@entry] let decrement (delta : int) (store : storage) : result = [],store - delta
  [@entry] let reset () (_ : storage) : result = [],0
end
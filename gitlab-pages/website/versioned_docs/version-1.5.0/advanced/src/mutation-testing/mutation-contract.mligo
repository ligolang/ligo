// This is mutation-contract.mligo
module C = struct
  type storage = int

  // Two entrypoints
  [@entry] let add (delta : int) (store : storage) : operation list * storage = [],store + delta
  [@entry] let sub (delta : int) (store : storage) : operation list * storage = [],store - delta
end
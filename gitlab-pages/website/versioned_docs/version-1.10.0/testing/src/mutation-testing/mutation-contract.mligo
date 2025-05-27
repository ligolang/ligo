// This is mutation-contract.mligo
module AddSub = struct
  type storage = int

  [@entry] let add (delta : int) (storage : storage) : operation list * storage = [], storage + delta
  [@entry] let sub (delta : int) (storage : storage) : operation list * storage = [], storage - delta
end
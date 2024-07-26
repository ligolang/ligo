// This is mycontract.mligo
module MyContract = struct
  type storage = int
  type result = operation list * storage

  [@entry] let increment (delta : int) (storage : storage) : result = [],storage + delta
  [@entry] let decrement (delta : int) (storage : storage) : result = [],storage - delta
  [@entry] let reset () (_storage : storage) : result = [], 0
end
module ContractA = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let increment (delta : int) (storage : storage_type) : return_type =
    [], storage + delta

  [@entry]
  let decrement (delta : int) (storage : storage_type) : return_type =
    [], storage - delta
end

module ContractB = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let add = ContractA.increment

  [@entry]
  let sub = ContractA.decrement
end
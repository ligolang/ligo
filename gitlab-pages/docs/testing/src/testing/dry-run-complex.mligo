module MyContract = struct
  type storage_type = (nat, string) map
  type return_type = operation list * storage_type

  [@entry]
  let update (param: nat * string) (storage: storage_type): return_type =
    let (index, value) = param in
    let updated_map = Map.add index value storage in
    [], updated_map

end
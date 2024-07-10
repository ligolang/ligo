module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let main (_action: unit) (storage: storage_type): return_type =
    [], storage + 1
end
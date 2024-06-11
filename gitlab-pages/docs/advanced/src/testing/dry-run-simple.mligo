module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  [@entry]
  let main (_action: unit) (store: storage_type): return_type =
    [], store + 1
end
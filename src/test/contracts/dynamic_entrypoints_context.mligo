type storage = { storage: int; dynamic_entrypoints }

type return_ = operation list * storage

module C = struct
  let f () = 42
  [@dyn_entry] let foo (_: unit) (s: storage): return_ = [], { s with storage = f () }
  let f () = 1432182439871329841732984721
  [@entry] let bar (_: unit) (s: storage): return_ = [],  { s with storage = -1 }
end

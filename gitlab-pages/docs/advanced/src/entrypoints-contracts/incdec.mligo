module IncDec = struct
  type storage = int
  type result = operation list * storage

  (* Three entrypoints *)
  [@entry] let increment (delta : int) (store : storage) : result =
    [], store + delta
  [@entry] let decrement (delta : int) (store : storage) : result =
    [], store - delta
  [@entry] let reset (() : unit) (_ : storage) : result =
    [], 0
end
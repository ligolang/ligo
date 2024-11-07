module IncDec = struct
  type storage = int
  type result = operation list * storage

  (* Four entrypoints *)
  [@entry] let increment (delta : int) (storage : storage) : result =
    [], storage + delta
  [@entry] let default (() : unit) (storage : storage) : result =
    increment 1 storage
  [@entry] let decrement (delta : int) (storage : storage) : result =
    [], storage - delta
  [@entry] let reset (() : unit) (_ : storage) : result =
    [], 0
end
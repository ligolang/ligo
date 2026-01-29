module Counter = struct
  type storage_type = int
  type return_type = operation list * storage_type

  (* Three entrypoints *)

  [@entry]
  let add (value : int) (store : storage_type) : return_type =
    [], store + value

  [@entry]
  let sub (value : int) (store : storage_type) : return_type =
    [], store - value

  [@entry]
  let reset (_p : unit) (_s : storage_type) : return_type =
    [], 0
end

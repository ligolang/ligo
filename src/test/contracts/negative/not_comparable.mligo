let main (_u : (int set) set) (s : unit) : operation list * unit =
  ([] : operation list), s

module Main2 = struct
  let main (_u : (int set) ticket) (s : unit) : operation list * unit =
    ([] : operation list), s

end

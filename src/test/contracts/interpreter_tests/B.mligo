
let f (p : unit) : unit = p

let main (_p : unit) (_s : unit) : (operation list) * unit =
  ([] : operation list), f ()


let f (p : unit) : unit = p

let main (_p, _s : unit * unit) : (operation list) * unit =
  ([] : operation list), f ()

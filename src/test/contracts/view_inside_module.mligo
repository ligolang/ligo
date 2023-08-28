module V = struct
  [@view]
  let v (_ : unit * unit) : unit = ()

end

[@entry]
let main () () : operation list * unit = ([] : operation list), ()

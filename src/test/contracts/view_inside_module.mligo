module V = struct
    [@view] let v (_: unit * unit) : unit = ()
end

let main (_ : unit * unit) : operation list * unit =
    ([] : operation list), ()
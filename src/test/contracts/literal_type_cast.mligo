let works = (0 : timestamp)

module Does_work = struct
  let does_work = (0 : timestamp)

end

[@entry]
let main (_ : unit) (_ : timestamp) : operation list * timestamp =
  (([] : operation list), works)

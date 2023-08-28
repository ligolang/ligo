let x =
  let a = 1 in
  module B = struct
    let y = a

  end in
  B.y

[@entry]
let main () (_ : int) : operation list * int = [], x

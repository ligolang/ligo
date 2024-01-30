module A = struct
  module B = struct
    type t = unit
  end
end

let x = (failwith "" : A.B.t)
let y = let x = (failwith "" : A.B.t) in x

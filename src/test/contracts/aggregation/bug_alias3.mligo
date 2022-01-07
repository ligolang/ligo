module A = struct
  let a = 1
end
module A_s = struct
  let as = 42
end

module B = struct
  module A = A
  let x = A.a
  module A = A_s
  let b = A.as
end

let x = B.A.as

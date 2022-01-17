module A = struct
  let a = 42
end

module B = struct
  module A = A
  let b = 1
end

let x = B.A.a

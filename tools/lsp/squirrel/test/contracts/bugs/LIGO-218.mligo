// 'module A = A' caused a loop when accessing 'B.A.a'.
// Futhermore, the second 'module A = A' caused a loop.
module A = struct
  let a: int = 42
end

module B = struct
  module A = A
  module A = A
end

let x = B.A.a

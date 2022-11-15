module A = struct
  let nested: int = 0
end

module B = A

let nested_not: int = 1
let complete_me: int = B.nest

module A = struct
  module B = struct
    let target = 0
  end

  module Y = B
end

module X = A

let target = X.Y.

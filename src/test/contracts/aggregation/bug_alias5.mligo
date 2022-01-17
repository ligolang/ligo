module A = struct
  let a = 1
  module A_s = struct
    let as = 42
  end
end


module B = struct
  module A = A
end


module A = struct
  module A_s = struct
    let as = 3
  end
end

let x = B.A.A_s.as

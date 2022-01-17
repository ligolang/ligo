module A = struct
  module B = struct
    let x = 42
  end
end


module A = struct
  module B = struct
    let x = 2
  end
  let y = A.B.x
end


let x = A.y

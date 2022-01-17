module A = struct
  module B = struct
    let x = 41
  end
end


module A = struct
  module B = struct
    let x = A.B.x + 1
  end
end

let x = A.B.x

module A = struct
  let v = 40
  module B = struct
    let v = v + 1
    module C = struct
      let v = v + 1
    end
  end
end

let x = A.B.C.v

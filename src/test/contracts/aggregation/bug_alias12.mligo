module F = struct
  module F = struct
    let a = 42
  end
  module F = struct
    let x = F.a
  end
end

let x = F.F.x

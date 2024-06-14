module A = struct
  let a1 = a2

  module B : sig val b end = struct
    let b1 = b2
  end

  let a3 = a1 B.b1
end

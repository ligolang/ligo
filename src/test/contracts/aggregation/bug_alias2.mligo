module A = struct
  let a = 40
end

module B = struct
  let b =
    module A = struct
      let ba = 1
      let baa = ba
    end in
    A.ba + A.baa
end

let x = A.a + B.b

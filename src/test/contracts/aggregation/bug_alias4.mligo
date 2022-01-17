module A_s = struct
  let as = 20
end
module A = struct
  let s_as = 22
end

let x = A_s.as + A.s_as

module M = struct
  type t = K
  type u = L of unit
  type v = M of u
end

let x1 = K
let y1 = x1

let x2 = L ()
let (L y2) = x2

let x3 = M x2
let (M y3) = x3
let (L z3) = y3

module N = struct
  type t = K of M.t
end

let x4 = K K
let (K y4) = x4
let (K z4) = y4

module O = struct
  include N
  type u = L of t
end

let x5 = L x4
let (L y5) = x5

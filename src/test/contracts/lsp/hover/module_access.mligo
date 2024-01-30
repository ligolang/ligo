module M = struct
  type t = int
  type u = unit
  let x : t = 3
end

let y = M.x

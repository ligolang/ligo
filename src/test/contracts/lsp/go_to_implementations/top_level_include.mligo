module M = struct
  let x : int = 42
end

module A = M

include A

let y = x

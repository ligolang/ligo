module M = struct
  let x = 1 + ""
  let y = x
end

let y : int = M.y

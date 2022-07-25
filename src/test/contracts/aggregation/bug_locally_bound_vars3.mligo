module Foo = struct
  let x = "hehe"
  let rec x(z : int) : int = if z = 0 then 1 else x (z - 1)
end

let test = Foo.x 3 + 1
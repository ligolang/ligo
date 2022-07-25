module Foo = struct
  let x = "hehe"
  let rec x(z : int) : int = x z
end

let test = Foo.x 42 + 1
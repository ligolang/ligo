module Foo = struct
  let x = 19
  let y = 22
end

let x =
  let x = 1 in
  module Toto = Foo in
  let u = Toto.x in
  let v = Toto.y in
  u + v + x

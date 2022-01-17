
module Foo = struct
  let x = 1
end


let foo =
  let x = 20 in
  module Foo = struct
    let x = x (* 20 *)
    let y = Foo.x (* 1 *)
    let z = y
  end in
  Foo.x + Foo.y + x + Foo.z

let x = foo

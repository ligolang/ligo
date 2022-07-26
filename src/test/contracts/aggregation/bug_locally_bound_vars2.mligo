module Foo = struct
  let x = "hehe"
  let id (z : int) : int = (fun (x : int) : int -> x) z
end

let test = Foo.id 42 + 1
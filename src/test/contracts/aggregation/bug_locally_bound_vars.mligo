module Foo = struct
  let x = "hehe"
  let foo (z : int option) : int = match z with
    | Some x -> x
    | None -> 42
end

let test = Foo.foo (Some 42)
let test2 = Foo.x

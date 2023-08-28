type storage =
  {
   x : int;
   y : int
  }

let foo (x : int) = x

let bar (x : int) = x * 9 + 3

[@entry]
let main (_ : int) (s : storage) =
  let x = s.x + 3 in
  let x = foo x in
  let x = bar s.x in
  ([] : operation list), {s with x = x}

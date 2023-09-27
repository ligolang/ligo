module Bar = struct
  let y (x : int) = 5 + x
end

module Foo = struct
  module Bar = struct
    let x = 54
  end
  let x = 55
end

let y () = Foo.Bar.x

let foo () =
  let x = 1 in
  module Foo = struct
    let x = x
  end in
  Foo.x

module Foo = struct
  let x = 2
  let x () = x + Foo.Bar.x
end

module C = struct
  [@entry]
  let main (_ : unit) (s : int) : operation list * int =
    let v = foo () + Foo.x () + y () in
    ([] : operation list), (s + v)
end

let test =
  let orig = Test.originate (contract_of C) 0 0tez in
  let _ = Test.transfer_exn orig.addr (Main ()) 0tez in
  Test.log (Test.get_storage orig.addr)

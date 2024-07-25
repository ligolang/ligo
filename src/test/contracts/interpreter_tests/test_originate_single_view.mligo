module Test = Test.Next

let () = Test.IO.unset_test_print ()

module Bar = struct
  module Foo = struct
    [@entry] let add n s : operation list * int = [], s + n
    [@entry] let sub n s : operation list * int = [], s - n
    [@view] let get () s : int = s
  end
end

let test =
  let orig = Test.Originate.contract (contract_of Bar.Foo) 0 0tez in
  let _ = Test.Typed_address.transfer_exn orig.taddr (Add 42) 0tez in
  ()

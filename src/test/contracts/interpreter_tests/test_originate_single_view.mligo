let () = Test.unset_print_values ()

module Bar = struct
  module Foo = struct
    [@entry] let add n s : operation list * int = [], s + n
    [@entry] let sub n s : operation list * int = [], s - n
    [@view] let get () s : int = s
  end
end

let test =
  let orig = Test.originate (contract_of Bar.Foo) 0 0tez in
  let _ = Test.transfer_exn orig.addr (Add 42) 0tez in
  ()

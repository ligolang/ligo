module Test = Test.Next

let println = Test.IO.println
let to_string = Test.String.show
let get_storage = Test.Typed_address.get_storage

let () = Test.IO.unset_test_print ()

module Bar = struct
  module Foo = struct
    [@entry] let add n s : operation list * int = [], s + n
    [@entry] let sub n s : operation list * int = [], s - n
    [@view] let get () s : int = s
    [@view] let get_diff k s : int = s - k
  end
end

let test =
  let {taddr = ta; code = m; size = _} =
    Test.Originate.contract (contract_of Bar.Foo) 0 0tez in
  let () = println "Deployed the contract:" in
  let () = println (to_string m) in
  let () = println ("With storage: " ^ to_string (get_storage ta)) in
  let c : (Bar.Foo parameter_of) contract =
    Test.Typed_address.to_contract ta in
  let _ = Test.Contract.transfer_exn c (Add 42) 0tez in
  let () = println ("Storage after call: " ^ to_string (get_storage ta)) in
  ()

let () = Test.unset_print_values ()

module Bar = struct
  module Foo = struct
    [@entry] let add n s : operation list * int = [], s + n
    [@entry] let sub n s : operation list * int = [], s - n
    [@view] let get () s : int = s
    [@view] let get_diff k s : int = s - k
  end
end

let test =
  let ta, m, _ = Test.originate_module (contract_of Bar.Foo) 0 0tez in
  let () = Test.println "Deployed the contract:" in
  let () = Test.println (Test.to_string m) in
  let () = Test.println ("With storage: " ^ Test.to_string (Test.get_storage ta)) in
  let c : (Bar.Foo parameter_of) contract = Test.to_contract ta in
  let _ = Test.transfer_to_contract_exn c (Add 42) 0tez in
  let () = Test.println ("Storage after call: " ^ Test.to_string (Test.get_storage ta)) in
  ()

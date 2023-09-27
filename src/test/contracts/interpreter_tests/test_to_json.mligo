let () = Test.unset_print_values ()
module C = struct
  type storage = { foo : int ; bar : string list }
  [@entry]
  let main (() : unit) (s : storage) : operation list * storage = [], s
end
let test_to_json =
  let orig = Test.originate (contract_of C) { foo = 42 ; bar = ["hello"; "world"] } 0tez in
  let () = Test.println (Test.to_json orig.addr) in
  let () = Test.println (Test.to_json (Test.get_storage orig.addr)) in
  ()

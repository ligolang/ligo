module Test = Test.Next

let println = Test.IO.println
let to_json = Test.String.json

let () = Test.IO.unset_test_print ()

module C = struct
  type storage = { foo : int ; bar : string list }
  [@entry]
  let main (() : unit) (s : storage) : operation list * storage = [], s
end

let test_to_json =
  let orig = Test.Originate.contract
               (contract_of C) { foo = 42 ; bar = ["hello"; "world"] } 0tez in
  let () = println (to_json orig.taddr) in
  let () = println (to_json (Test.Typed_address.get_storage orig.taddr)) in
  ()

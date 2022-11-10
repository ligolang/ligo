let () = Test.unset_print_values ()

type storage = { foo : int ; bar : string list }

let main (((), s) : unit * storage) : operation list * storage = [], s

let test_to_json =
  let (ta, _, _) = Test.originate main ({ foo = 42 ; bar = ["hello"; "world"] } : storage) 0tez in
  let () = Test.println (Test.to_json ta) in
  let () = Test.println (Test.to_json (Test.get_storage ta)) in
  ()

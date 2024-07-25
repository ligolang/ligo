let assert = Assert.assert

module Test = Test.Next

let test =
  let tester (a, _, _) : unit =
    (* Test 1 *)
    let _ = Test.Typed_address.transfer_exn a 0 0tez in
    let () = assert (Test.Typed_address.get_storage a = 0) in
    (* Test 2 *)
    let _ = Test.Typed_address.transfer_exn a 1 0tez in
    let () = assert (Test.Typed_address.get_storage a = 1) in
    ()
  in
  let fn = "adder.mligo" in
  Test.Mutation.All.from_file fn 0 0tez tester

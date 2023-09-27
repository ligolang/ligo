let test =
  let tester (a, _, _) : unit =
    (* Test 1 *)
    let _ = Test.transfer_exn a 0 0tez in
    let () = assert (Test.get_storage a = 0) in
    (* Test 2 *)
    let _ = Test.transfer_exn a 1 0tez in 
    let () = assert (Test.get_storage a = 1) in
    ()
  in
  let fn = "adder.mligo" in
  Test.originate_from_file_and_mutate_all fn 0 0tez tester

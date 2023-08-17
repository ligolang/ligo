let tester ((a, _, _) : address * michelson_contract * int) : unit =
  (* Test 1 *)
  let _ = Test.transfer_exn a (Test.eval 0) 0tez in
  let () = assert (Test.get_storage_of_address a = (Test.eval 0)) in
  (* Test 2 *)
  let _ = Test.transfer_exn a (Test.eval 1) 0tez in 
  let () = assert (Test.get_storage_of_address a = (Test.eval 1)) in
  ()

let test =
    let fn = "adder.mligo" in
    let e = "main" in
    Test.originate_from_file_and_mutate_all fn e (Test.eval 0) 0tez tester

module PBT = Test.PBT

(* Let's check if it really is *)
let test =
  (* We generate the property *)
  let test = PBT.make_test (PBT.gen_small : ((int contract) list) gen) (fun (xs : (int contract) list) -> List.length xs = 42n) in
  (* And run it *)
  match PBT.run test 10n with
  | Fail _counter_example -> ()
  | Success -> ()

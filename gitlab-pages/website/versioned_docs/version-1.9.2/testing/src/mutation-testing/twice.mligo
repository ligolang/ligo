let twice (x : int) = x + x
module Test = Test.Next
let simple_tests (f : int -> int) =
  (* Test 1 *)
  let () = Assert.assert (Test.Compare.eq (Test.Michelson.run f 0) (Test.Michelson.eval 0)) in
  (* Test 2 *)
  let () = Assert.assert (Test.Compare.eq (Test.Michelson.run f 2) (Test.Michelson.eval 4))
  in ()

let test = simple_tests twice
let test_mutation =
  match Test.Mutation.func twice simple_tests with
    None -> ()
  | Some (_, mutation) ->
      let () = Test.IO.log mutation in
      Test.IO.println "Some mutation also passes the tests! ^^"
let get_all_mutations =
  match Test.Mutation.All.func twice simple_tests with
    [] -> ()
  | ms ->
      let () = Test.IO.println "Some mutation also passes the tests! ^^" in
      List.iter (fun ((_, mutation) : unit * mutation) -> Test.IO.log mutation) ms
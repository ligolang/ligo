#include "./gitlab-pages/docs/testing/src/testing/remove-balance.mligo"

let test_remove_balance =
  let () = Test.Next.State.reset 5n ([]: tez list) in
let balances: balances =
  let a1, a2, a3 = Test.Next.Account.address 1n, Test.Next.Account.address 2n, Test.Next.Account.address 3n
    in Map.literal [(a1, 10tz); (a2, 100tz); (a3, 1000tz)] in
  List.iter
    (fun ((threshold , expected_size): tez * nat) ->
      let tester (balances, threshold: balances * tez) = Map.size (remove_balances_under balances threshold) in
      let size = Test.Next.Michelson.run tester (balances, threshold) in
      let expected_size = Test.Next.Michelson.eval expected_size in
      let () = Test.Next.IO.log ("expected", expected_size) in
      let () = Test.Next.IO.log ("actual", size) in
      Assert.assert (Test.Next.Compare.eq size expected_size)
    )
    [(15tez,2n); (130tez,1n); (1200tez,0n)]
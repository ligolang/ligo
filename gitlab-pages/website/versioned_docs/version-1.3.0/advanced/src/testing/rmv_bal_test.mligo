#include "./gitlab-pages/docs/advanced/src/testing/remove-balance.mligo"
let _u = Test.reset_state 5n ([] : tez list)
let balances : balances =
  let a1, a2, a3 = Test.nth_bootstrap_account 1, Test.nth_bootstrap_account 2, Test.nth_bootstrap_account 3
  in Map.literal [(a1, 10tz); (a2, 100tz); (a3, 1000tz)]
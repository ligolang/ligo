#import "common.mligo" "Common"

let mult a b = a * b

let config =
  { parameter =
      let a = mult 100 2 in
      let b = Common.some_string in
      { a; b }
  ; storage = ()
  ; entrypoint = Common.entrypoint
  ; contract_env =
      { amount = Common.some_tez
      }
  }

module Tezos = Tezos.Next

let check (p,s : unit * tez) = [], Tezos.get_balance()
let threshold (p : unit) = if Tezos.get_amount () = 100tz then 42 else 0
let check (p : unit) = Tezos.self("%default")
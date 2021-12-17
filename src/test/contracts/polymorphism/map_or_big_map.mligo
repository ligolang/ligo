(* reduced version from bug report *)
type tokenValue = int

type state =  Build_state of (address, tokenValue) map

let balances (s : state) = match s with Build_state x -> x

let try_transfer (from : address) (amount0 : tokenValue) (state : state) =
  let from_balance = 0 in
  let new_balances = Map.add from (from_balance + amount0) (balances state) in 
  Some (Build_state new_balances)
let nats = [0; 1; 2; 3; 4]
let evens_zeroed = List.update_with (fun x -> x mod 2 = 0n) 0 nats
// evens_zeroed = [0; 1; 0; 3; 0]
let f x = if x mod 2 = 0n then None else Some (x*x)
let odds_squared = List.update f nats
// odds_squared = [0; 1; 2; 9; 4]
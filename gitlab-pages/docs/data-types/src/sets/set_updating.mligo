let nats : int set = Set.literal [3; 2; 2; 1]
let set_with_5 = Set.update 5 true nats
let set_without_3 = Set.update 3 false nats
let f x = if x mod 2 = 0n then None else Some x
// odds = Set.literal [3, 1]
let odds = Set.filter_map f nats
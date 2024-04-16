let nats : int big_set = Big_set.literal [3; 2; 2; 1]
let big_set_with_5 = Big_set.update 5 true nats
let big_set_without_3 = Big_set.update 3 false nats
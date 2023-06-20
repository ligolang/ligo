let x = 2

let y = [%Michelson ({| { FAILWITH } |} : nat * bytes -> address)]

let z = x, x

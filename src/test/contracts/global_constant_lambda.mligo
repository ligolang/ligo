type storage = int * (int -> int)

type parameter = int -> int

type return = operation list * storage

let i =
  (Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2"
   : int -> int)

let s = (1, i)

[@entry]
let main (f : parameter) ((k, g) : storage) : return =
  ([] : operation list), (g k, f)

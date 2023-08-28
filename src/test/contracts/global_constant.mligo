type storage = int

type parameter = unit

type return = operation list * storage

let v =
  (Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2"
   : int -> int)
    42

[@entry]
let main (() : parameter) (store : storage) : return =
  ([] : operation list),
  ((Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2"
    : int -> int)
     store)

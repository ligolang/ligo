type storage = int
type parameter = unit
type return = operation list * storage

let main ((), store : parameter * storage) : return =
 ([] : operation list), ((Tezos.constant "expruCKsgmUZjC7k8NRcwbcGbFSuLHv5rUyApNd972MwArLuxEZQm2" : int -> int) store)

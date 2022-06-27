#import "@ligo/bigarray-cameligo/lib/bigarray.mligo" "BigArray"

type big_array = BigArray.big_array

type parameter = unit
type storage = int big_array

type return = operation list * storage

let main (_, s : unit * storage) : return =
  [], BigArray.reverse s
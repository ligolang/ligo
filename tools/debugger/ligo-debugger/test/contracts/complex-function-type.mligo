let foo (a : int) : nat option -> (int * string) -> int = failwith "kek"

[@entry]
let main () (s : int) : operation list * int =
  (([] : operation list), foo s None (1000 - 7, "oshiete"))

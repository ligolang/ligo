type storage = int

let%entry main (p:int) storage =
  ((list [] : operation list) , p + storage)

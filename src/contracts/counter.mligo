type storage = int

let%entry main (p:int) storage =
  (([] : operation list) , p + storage)

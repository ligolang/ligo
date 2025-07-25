let sum (x, y : int * int) = x + y

let main (parameter, storage : int * int) : operation list * int =
  ([], sum (parameter, storage))
let rec recursive (n, acc : int * int) : int =
  if n = 0 then acc else recursive(n - 1, acc + 2)

[@entry]
let main () (s : int) : operation list * int =
  (([] : operation list), recursive(s, 0))

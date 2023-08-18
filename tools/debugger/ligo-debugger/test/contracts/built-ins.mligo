[@entry]
let main (_, s : unit * int) : operation list * int =
  let my_set : int set = Set.literal [3; 2; 2; 1] in
  let sum (acc, i : int * int) : int = acc + i in
  let sum_elems : int = Set.fold sum my_set 0 in
  (([] : operation list), s + sum_elems)

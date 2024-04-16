type player = string
type abscissa = int
type ordinate = int
type move = abscissa * ordinate
type game = (player, move) map

let horizontal_offset (g : game) : int =
  let folded = fun (acc, j : int * (player * move)) -> acc + j.1.0
  in Map.fold folded g 0
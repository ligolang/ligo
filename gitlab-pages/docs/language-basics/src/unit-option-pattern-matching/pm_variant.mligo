type color =
  | RGB   of int * int * int
  | Gray  of int
  | Default

let int_of_color (c : color) : int =
  match c with
  | RGB (r,g,b) -> 16 + b + g * 6 + r * 36
  | Gray i -> 232 + i
  | Default -> 0
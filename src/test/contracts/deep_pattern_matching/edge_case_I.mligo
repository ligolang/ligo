type t =
| A0
| A1
| A2
| A3
| A4
| A5
| A6
| A7
| A8
| A9
| A10

[@entry]
let main (p : t) (_ : int) : operation list * int =
  [],
  (match p with
     A0 -> 0
   | A1 -> 1
   | A2 -> 2
   | A3 -> 3
   | A4 -> 4
   | A5 -> 5
   | A6 -> 6
   | A7 -> 7
   | A8 -> 8
   | A9 -> 9
   | A10 -> 10)


(*
  Here, the two tuples have no types in common and different sizes.
  The diff should display deletion of all elements of first tuple
  and insertion of all elements of the second.

  TODO NP :
  Instead of display - + - +... :
    - string
    + tez
    - int
    + nat
    - int
    + tez
    - string
  we want to display instead :
    - string
    - int
    - int
    + tez
    + nat
    + tez
    - string
  i.e., consecutive changes
    CHANGE A1 TO B1; CHANGE A2 TO B2
  shouldn't appear as 
    DELETE A1; INSERT B1; DELETE A2; INSERT B2
  but instead :
    DELETE A1; DELETE A2; INSERT B1; INSERT B2

*)
let main (_p, s : int * int) : operation list * int =
  let  x = "foo", 42, 24, "bar" in
  let _y : tez * nat * tez = x in
  ([] : operation list), s
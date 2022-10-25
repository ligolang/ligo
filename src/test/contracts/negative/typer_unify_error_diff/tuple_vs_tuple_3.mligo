
(*
  Here, the two tuples have 4 changes and different sizes.
  The diff should display these changes.
*)
let main (_p, s : int * int) : operation list * int =
  let  x =  "foo" , 42  , 24n , 42 ,        "bar",  42 in
  let _y : (tez   * int       * tez * nat * string)     = x in
  //        ^^^^^         ^^^         ^^^           ^^
  //       changed      removed      added          added
  ([] : operation list), s

(*
  Here, the two tuples have some changes (1 change, 1 addition, 1 deletion)
  but they have the *same size*, so the typer will only display an error
  on the first difference only (here, [string] vs. [tez])
*)
let main (_p, s : int * int) : operation list * int =
  let  x =  "foo" , 42  , 24n , 42 ,        "bar" in
  let _y : (tez   * int       * tez * nat * string) = x in
  //        ^^^^^         ^^^         ^^^   
  //       changed      removed      added
  ([] : operation list), s
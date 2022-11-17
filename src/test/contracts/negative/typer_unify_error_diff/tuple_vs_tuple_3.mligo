
let main (_p, s : int * int) : operation list * int =
  let  x : string * int * nat * tez *       string * int =  "foo" , 42  , 24n , 42tez ,        "bar",  42 in
  let _y : tez    * int       * tez * nat * string       = x in
  //       ^^^^^^         ^^^         ^^^            ^^^
  //       changed        removed     added          removed 
  ([] : operation list), s
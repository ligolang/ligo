let () = Test.set_print_values ()

type r = { a : nat , b : int  , c : string }

let f = () => {
  Test.log ("Once");
  ( { a : 1n , b : 1 , c : "H" }
  , { a : 2n , b : 2 , c : "E" }
  , { a : 3n , b : 3 , c : "L" } 
  )
}

let ( { a : a1 , b : b1 , c : c1 }
    , { a : a2 , b : b2 , c : c2 }
    , { a : a3 , b : b3 , c : c3 }
    ) = f ()
let ( { a : a4 , b : b4 , c : c4 }
    , { a : a5 , b : b5 , c : c5 }
    , { a : a6 , b : b6 , c : c6 }
    ) 
  = ( { a : 1n , b : 1 , c : "H" }
    , { a : 2n , b : 2 , c : "E" }
    , { a : 3n , b : 3 , c : "L" } 
    )

let test = {
  assert (a1 + a2 + a3 == a4 + a5 + a6);
  assert (b1 + b2 + b3 == b4 + b5 + b6);
  assert (c1 ++ c2 ++ c3 == c4 ++ c5 ++ c6)
}
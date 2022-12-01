let () = Test.set_print_values ()

type r = 
  { a : (nat    , int , string)
  , b : (int    , nat , string) 
  , c : (string , int , nat)
  }

let f = () => {
  Test.log ("Once");
  { a : (1n     , 1 , "H") 
  , b : (2      , 2n, "E") 
  , c : ("Hello", 3 , 3n ) 
  }
}
let { a : (a1, a2, a3)
    , b : (b1, b2, b3)
    , c : (c1, c2, c3)
    } = f ()

let 
  { a : (a4, a5, a6)
  , b : (b4, b5, b6)
  , c : (c4, c5, c6)
  } = 
  { a : (1n     , 1 , "H") 
  , b : (2      , 2n, "E") 
  , c : ("Hello", 3 , 3n ) 
  }

let test = {
  assert ((a1 + b2 + c3) == (a4 + b5 + c6));
  assert ((b1 + a2 + c2) == (a5 + b4 + c5));
  assert ((a3 ++ b3 ++ c1) == (a6 ++ b6 ++ c4))
}
const Unit = Test.set_print_values ()

type r is record[ a : nat ; b : int  ; c : string ]

function f (const _ : unit) is {
  Test.log ("Once");
} with
  ( record[ a = 1n ; b = 1 ; c = "H" ]
  , record[ a = 2n ; b = 2 ; c = "E" ]
  , record[ a = 3n ; b = 3 ; c = "L" ] 
  )

const 
    ( record[ a = a1 ; b = b1 ; c = c1 ]
    , record[ a = a2 ; b = b2 ; c = c2 ]
    , record[ a = a3 ; b = b3 ; c = c3 ]
    ) = f (Unit)

const 
    ( record[ a = a4 ; b = b4 ; c = c4 ]
    , record[ a = a5 ; b = b5 ; c = c5 ]
    , record[ a = a6 ; b = b6 ; c = c6 ]
    ) 
  = ( record[ a = 1n ; b = 1 ; c = "H" ]
    , record[ a = 2n ; b = 2 ; c = "E" ]
    , record[ a = 3n ; b = 3 ; c = "L" ]  
    )

const test = {
  assert (a1 + a2 + a3 = a4 + a5 + a6);
  assert (b1 + b2 + b3 = b4 + b5 + b6);
  assert (c1 ^ c2 ^ c3 = c4 ^ c5 ^ c6);
} with Unit
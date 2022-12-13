const Unit = Test.set_print_values ()

type r is record[ a : nat ; b : int ; c : string ]

function f (const _ : unit) is {
  Test.log ("Once");
} with record[ a = 1n ; b = 1 ; c = "Hello" ]

const record[ a ; b ; c ] = f (Unit)
const record[ a = a1 ; b = b1 ; c = c1 ] = 
      record[ a = 1n ; b = 1 ; c = "Hello" ]

const test = {
  assert (a = a1);
  assert (b = b1);
  assert (c = c1);
} with Unit
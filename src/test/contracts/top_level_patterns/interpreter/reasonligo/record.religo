let () = Test.set_print_values ()

type r = { a : nat , b : int , c : string }

let f = () => {
  Test.log ("Once");
  { a : 1n , b : 1 , c : "Hello" }
}
let { a , b , c } = f ()
let { a : a1 , b : b1 , c : c1 } = { a : 1n , b : 1 , c : "Hello" }

let test = {
  assert (a == a1);
  assert (b == b1);
  assert (c == c1)
}
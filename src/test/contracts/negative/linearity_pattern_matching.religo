type foo = { a : int , b : nat , c : string };

let yy : string = switch ({ a : 1 , b : 2n , c : "33" }) {
  | { a : a ,  b : b , c : a } => a
  }
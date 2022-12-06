type r = { a : nat ; b : int ; c : string }

let r = { a = 1n ; b = 1 ; c = "Hello" }
let { a ; b = a ; c } = r
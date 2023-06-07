#include "../simple.mligo"
#import "../local_module.mligo" "A"
let format_me =
  let x = 20 in
  x * 2

let format_me_2 =
  let q = A.A.s in
  q ^ q

let format_me_long = "abc" ^ "def" 
^ "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa" 
^ "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"

type my_record = {a : int; b : nat; c : string}
type my_tuple = int * nat * string

let on_record (v : my_record) : int =
  match v with
    { a ; b = b_renamed ; c = _ } -> a + int b_renamed

let on_tuple (v : my_tuple) : int =
  match v with (x , y, _) -> x + int y
type some_r = [@layout comb] {
  one : int;
  two : nat;
  three : string;
  four : bytes;
  five : unit
}

let f = fun (x : some_r) -> x.one

let test_example =
  Test.run (fun (x : int * nat * string * bytes * unit) -> f ({ one = x.0 ; two = x.1 ; three = x.2 ; four = x.3 ; five = x.4 }))
           (1 + 3 + 2, 1n + 2n, "a" ^ "b", 0xFF00, ())
type some_r = [@layout comb] {
  one   : int;
  two   : nat;
  three : string;
  four  : bytes;
  five  : unit
}

let f = fun (x : some_r) -> x.one

let test_example =
  Test.run (fun (x : int * nat * string * bytes * unit) -> f ({ one = x.0 ; two = x.1 ; three = x.2 ; four = x.3 ; five = x.4 }))
           (1 + 3 + 2, 1n + 2n, "a" ^ "b", 0xFF00, ())
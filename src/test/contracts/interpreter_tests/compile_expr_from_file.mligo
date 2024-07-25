module Test = Test.Next
let assert = Assert.assert

let test1 =
  type r_comb = [@layout comb] { one : int ; two : nat ; three : string ; four : bytes ; five : unit } in
  let d_one = Test.Michelson.eval (1 + 3 + 5) in
  let v = {one = 1 + 3 + 5 ; two = 1n +2n ; three = "a"^"b" ; four = 0xFF00 ; five = ()} in
  let ret = Test.Michelson.run (fun (x:r_comb) -> x.one ) v in
  assert (Test.Compare.eq d_one ret)

let test2 =
  let x1 = Test.Michelson.eval (1,2) in
  let x2 = Test.Michelson.run (fun () -> (1,2)) () in
  let eq = Test.Compare.eq x1 x2 in
  assert eq

let test3 =
  let x1 =
    type r_comb = [@layout comb] { one : int ; two : nat ; three : string ; four : bytes ; five : unit } in
    Test.Michelson.run (fun () -> ({one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = ()}:r_comb)) ()
  in
  let x2 =
    type r_tree = [@layout tree] { one : int ; two : nat ; three : string ; four : bytes ; five : unit } in
    Test.Michelson.run (fun () -> ({one = 1 ; two = 2n ; three = "a" ; four = 0xFF00 ; five = ()}:r_tree)) ()
  in
  assert (not (Test.Compare.eq x1 x2))

let test4 =
  let x1 =
    type v_comb = [@layout comb] | Foo of int | Bar of string | Bare of string | Baz of nat in
    Test.Michelson.run (fun () -> (Baz 1n: v_comb)) ()
  in
  let x2 =
    type v_tree = [@layout tree] | Foo of int | Bar of string | Bare of string | Baz of nat in
    Test.Michelson.run (fun () -> (Baz 1n: v_tree)) ()
  in
  assert (not (Test.Compare.eq x1 x2))

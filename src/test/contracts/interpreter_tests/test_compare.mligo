type t = [@layout:tree] | D of int | C of string
type c = [@layout:comb] | B of int | A of string

type cl = c list

let test_cmp =
  let () = Test.assert (A "hello" > B 42) in
  let () = Test.assert (C "x" < D 0) in
  ()

let test_cmp_list =
  let () = Test.assert ([A "hello" ; A "bye"] > [A "hello" ; B 42]) in
  ()

type rt = [@layout:tree] { b : int ; a : string }
type rc = [@layout:comb] { d : int ; c : string }

let test_cmp_record =
  let () = assert ({ a = "x" ; b = 0 } < { a = "y" ; b = 1 }) in
  let () = assert ({ c = "y" ; d = 0 } < { c = "x" ; d = 1 }) in
  ()

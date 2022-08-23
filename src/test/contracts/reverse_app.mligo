(*
  This test file stresses the reverse-application [|>] operator.

  The operator [|>] is supposed to work as function application,
  with argument on the left and function on the right.
  (The opposite order of classic function application)
  For example, [x |> f |> g] should behave as [g (f x)]

  This contract tests the equivalence of various expression,
  with versus without reverse-app operator.
  In all cases below, [my_expr1] and [my_expr2] should be equivalent

*)


let f            (x : int) : nat =   abs x
let g            (x : nat) : nat = 42n + x
let gg (y : nat) (x : nat) : nat =   y + x
let h            (x : nat) : int =   int x

let x = 24

(* One reverse-app operator *)
let test =
  let a = f x
  in
  let b = x |> f
  in
  assert (a = b)

(* Chaining reverse-app operators *)
let test =
  let a = h (g (f x))
  in
  let b = x |> f |> g |> h
  in
  assert (a = b)

(* Combining revere-app and partial application *)
let test =
  let a = h (gg 42n (f x))
  in
  let b = x |> f |> gg 42n |> h
  in
  assert (a = b)

(* Checking precedence with classical function application *)
let test =
  let a = g (f x)
  in
  let b = f x |> g
  in
  assert (a = b)


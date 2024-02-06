type ctor = Foo of int

type r = { a : int; b : { c : bool; d : string } }

type ('a, 'b) result = Ok of 'a | Error of 'b

type 'a poly_record = { x : int; y : 'a }

let ((Foo x), { a = _; b = { c = _; d = _ } } : ctor * r) =
  let (Foo _) = Foo 42 in
  let mut (Foo _) = Foo 42 in
  let a : (int, unit) result = Ok 42 in
  let _ =
    match a with
    | Ok x -> x
    | Error -> 0
  in
  let r : r = { a = 42; b = { d = "42"; c = true } } in
  let _ = r.b.d in
  let _ = { r with a = 100; b = { d = "24"; c = false } } in
  let pr : bool poly_record = { x = 42; y = true } in
  let _ : int poly_record = { x = 42; y = 24 } in
  let _ = pr.y in
  Foo 42, { a = 42; b = { d = "42"; c = true } }

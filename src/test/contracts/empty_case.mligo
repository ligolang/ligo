type foo =
  Bar of int
| Baz

let check (f : foo) : int =
  match f with
    Bar i -> i
  | Baz -> -1

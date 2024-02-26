type 'a foo = Foo of 'a

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

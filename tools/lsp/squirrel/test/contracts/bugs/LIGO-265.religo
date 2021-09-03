let funny = (a: list (Foo | Bar)): int => 1;

let lol = (a: int): int => funny([Bar]);

type x =
  | A
  | B(int)
  | C(X(int) | Y(int));

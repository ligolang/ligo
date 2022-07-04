function foo(const _u : unit) is
  {
    const (x, y) = (4, 5);
    x := 2;
    y := 3;
  } with (x + y)

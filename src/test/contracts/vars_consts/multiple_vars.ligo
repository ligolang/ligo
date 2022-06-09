function foo(const _u : unit) is
  {
    var (x, y) := (4, 5);
    x := 2;
    y := 3;
  } with (x + y)

function bar(const _u : unit) is
  {
    const (x, y) = (4, 5);
    function add(const _u : unit) is (x + y);
  } with add(unit)

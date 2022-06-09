function foo(const x : int) : int is
  {
    function bar(var x : int) : int is
      { x := x + 1; } with x;
  } with bar(42)

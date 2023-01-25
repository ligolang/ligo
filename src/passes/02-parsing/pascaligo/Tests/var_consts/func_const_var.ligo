function foo(const x : int) : int -> int is
  {
    function bar(var y : int) : int is x + y;
  } with bar

function foo(var x : int; const y : int) : int -> int is
  {
    function bar(const _ : unit) : int is x + y;
  } with bar

function foo(var x : int) : int is
  {
    function bar(const _ : unit) : int is x;
  } with bar

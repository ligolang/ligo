function foo (const i : int) : int is
  block {
    function bar (const i : int) : int is i
  } with bar (0)

function foo (const _i : int) : int is
  {
    function bar (const _i : int) : int is _i
  } with bar (0)

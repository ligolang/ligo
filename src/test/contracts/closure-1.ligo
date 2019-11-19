function foo (const i : int) : int is
  block {
    function bar (const j : int) : int is
      i + j ;
  } with bar (i)

function foo (const i : int) : int is
  function bar (const i : int) : int is
    block { skip } with i ;
  block { skip } with bar (0)

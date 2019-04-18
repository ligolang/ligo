function foo (const i : int) : int is
  function bar (const j : int) : int is
    block { skip } with i + j ;
  block { skip } with bar (i)


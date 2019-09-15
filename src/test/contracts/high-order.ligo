function foobar (const i : int) : int is
  function foo (const i : int) : int is
    block { skip } with i ;
  function bar (const f : int -> int) : int is
    block { skip } with f ( i ) ;
  block { skip } with bar (foo) ;

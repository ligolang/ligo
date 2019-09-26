function foobar(const i : int) : int is
  const j : int = 3 ;
  function toto(const k : int) : int is
    block { skip } with i + j + k ;
  block { skip } with toto(42)

function foobar(const i : int) : int is
  const j : int = 3 ;
  const k : int = 4 ;
  function toto(const l : int) : int is
    block { skip } with i + j + k + l;
  block { skip } with toto(42)

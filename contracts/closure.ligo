function foo (const i : int) : int is
  function bar (const j : int) : int is
    block { skip } with i + j ;
  block { skip } with bar (i)

function toto (const i : int) : int is
  function tata (const j : int) : int is
    block { skip } with i + j ;
  function titi (const j : int) : int is
    block { skip } with i + j ;
  block { skip } with tata(i) + titi(i)

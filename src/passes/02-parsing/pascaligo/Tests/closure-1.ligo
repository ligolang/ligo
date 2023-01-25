function foo (const i : int) : int is
  {
    function add (const j : int) : int is i+j
  } with add (i)

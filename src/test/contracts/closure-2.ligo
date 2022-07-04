function foobar (const i : int) : int is
  {
    const j : int = 3;
    function add (const k : int) : int is i+j+k
  } with add (42)

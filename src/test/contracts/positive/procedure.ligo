function my_assert (const p : bool) : unit is
  { if not p then failwith("assert") }
  with unit

function main (const p : bool; const _s : int) : list(operation) * int is
  {
    my_assert (p) ;
    const n : int = 4 ;
  } with ((nil : list(operation)), n)

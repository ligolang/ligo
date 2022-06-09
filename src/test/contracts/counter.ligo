type t is int

function main (const p : int; const s : t) : list (operation) * int is
  ((nil : list (operation)), p+s)

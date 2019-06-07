function f (const x : unit) : unit is
  begin skip end with unit

function main (const p : unit ; const s : unit) : unit is
  var y : unit := f(unit) ;
  begin skip end with y

function f (const x : unit) : unit is
  begin skip end with unit

function main (const p : unit ; const s : unit) : unit is
  begin
    var y : unit := f(unit) ;
  end with y

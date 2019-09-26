function f (const x : unit) : unit is
  begin skip end with unit

function main (const p : unit ; const s : unit) : unit is
  behin skip end with f unit
// the srcloc is correct but the reported term is "skip" instead of "behin".

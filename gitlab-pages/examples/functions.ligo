function multiply (const a : int ; const b : int) : int is
    begin
        const result : int = a * b ;
    end with result

function add (const a : int ; const b : int) : int is
    block { skip } with a + b

function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {skip} with ((nil : list(operation)), s)
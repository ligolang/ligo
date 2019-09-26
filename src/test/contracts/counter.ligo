type some_type is int

function main (const p : int ; const s : some_type) : (list(operation) * int) is
  block { skip } // skip is a do nothing instruction, needed for empty blocks
  with ((nil : list(operation)), p + s)


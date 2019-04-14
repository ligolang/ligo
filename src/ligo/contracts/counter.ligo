function main (const p : int ; const s : int) : (list(operation) * int) is
  block {skip} with ((nil : operation), p + s)

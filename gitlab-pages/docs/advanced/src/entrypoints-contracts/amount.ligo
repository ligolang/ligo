function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {
      if amount > 0mutez then failwith("This contract does not accept tez") else skip
  } with ((nil : list(operation)), unit);
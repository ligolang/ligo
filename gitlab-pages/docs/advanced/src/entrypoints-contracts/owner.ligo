const owner: address = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address);
function main (const p : unit ; const s : unit) : (list(operation) * unit) is
  block {
      if source =/= owner then failwith("This address can't call the contract") else skip
  } with ((nil : list(operation)), unit);
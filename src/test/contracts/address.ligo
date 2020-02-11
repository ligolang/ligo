// function main (const c : contract (unit)) : address is address (c)

function main (const p : key_hash) : address is block {
  const c : contract (unit) = implicit_account (p);
} with address (c)
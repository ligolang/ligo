function main (const p : key_hash) : address is {
  const c : contract (unit) = Tezos.implicit_account (p);
} with Tezos.address (c)

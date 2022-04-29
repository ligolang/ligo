function main (const p : key_hash) : list (operation) is
  block {
    const _unused : operation = Tezos.set_delegate (Some (p));
    const dummy : list (operation) = nil
  } with dummy

function main (const p : key_hash) : list (operation) is
  block {
    const unused : operation = set_delegate (Some (p));
    const dummy : list (operation) = nil
  } with dummy

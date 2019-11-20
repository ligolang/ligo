function cb(const a : address; const s : unit) : list(operation) * unit is
  block {
    const c : contract(unit) = get_entrypoint("%cb", a)
  }
  with (list transaction(unit, 0mutez, c) end, s)

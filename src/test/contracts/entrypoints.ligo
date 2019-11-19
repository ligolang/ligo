function cb(const a : address; const s : unit) : list(operation) * unit is
  const c : contract(unit) = get_entrypoint("%cb", a) ;
  block { skip }
  with (list transaction(unit, 0mutez, c) end, s)

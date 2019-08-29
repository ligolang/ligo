type storage_ is big_map(int, int) * unit

function main(const p : unit; const s : storage_) : list(operation) * storage_ is
  block { skip }
  with ((nil : list(operation)), s)
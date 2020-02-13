type storage is record
  one: map(key_hash, nat);
  two: big_map(key_hash, bool);
end

type return is list (operation) * storage

function main (const a : int; const store : storage) : return is ((nil: list(operation)), store)
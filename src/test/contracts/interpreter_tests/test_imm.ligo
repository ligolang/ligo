type storage is int;

type return is list (operation) * storage

function reset (var s : storage) : storage is {
  s := 0;
} with (s);

function main (const _action : unit; const s : storage) : return is
 ((nil : list (operation)), reset(s));

const test_orig = {
  const (_typed_address, _, _) = Test.originate(main, 42, 0tez);
} with (Unit)

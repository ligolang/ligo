// This is mockup_testme.ligo
type storage is string

type parameter is
  Append of string

type return is list (operation) * storage

function main (const action : parameter; const store : storage) : return is
 ((nil : list (operation)),    // No operations
  case action of
    Append (s) -> store ^ s
  end)

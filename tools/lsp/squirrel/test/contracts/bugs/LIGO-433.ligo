type storage is int

type parameter is
    First of record [a : int]
  | Second of record [a : int]
type return is list (operation) * storage

function foo (var x : record [a : int]) : int is x.a

function main (const action : parameter; const _ : storage) : return is
 ((nil : list (operation)),
  case action of
    First (n) -> foo (n)
  | Second (n) -> foo (n)
  end)
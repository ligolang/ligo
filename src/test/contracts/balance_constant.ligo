(**

This test makes sure that the balance is accessible in PascaLIGO.
It's there to detect a regression of: https://gitlab.com/ligolang/ligo/issues/68

*)

type storage is tez

function main (const p : unit; const s: int) : list(operation) * storage is
  ((nil : list(operation)), balance)

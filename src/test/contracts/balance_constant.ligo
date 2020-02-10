(*
This test makes sure that the balance is accessible in PascaLIGO.

It is meant to detect the regression detailled in the following issue: https://gitlab.com/ligolang/ligo/issues/68
*)

type parameter is unit
type storage is tez
type return is list (operation) * storage

function main (const param : parameter; const store: storage) : return is
  ((nil : list (operation)), balance)

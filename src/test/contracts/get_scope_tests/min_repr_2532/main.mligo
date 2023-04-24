#include "parameter.mligo"

let main (param, store : parameter * nat) = param, store
(*                       ^^^^^^^^^ *)
(* Issue solved by MR 2532 :
The "go-to-definition" for 'parameter' didn't do anything.
*)

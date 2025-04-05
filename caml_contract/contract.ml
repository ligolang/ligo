[@@@ligo]

(* contract *)
type storage =
  | A
  | B
  | C

type return = operation list * storage

module M = struct
  let u = ()
end

let a = [%nat 1]
let b = [%int 2]
let c = [%tez 3]
let d = [%mutez 4]
let e = [%address "KT1MNN3eEAu3nfKq8u814KfMGuwap7qZ7LPv"]
let add (x : nat) (y : nat) : nat = [%ligo.constant ADD (x, y)]
let next () (storage : storage) : return = [], storage

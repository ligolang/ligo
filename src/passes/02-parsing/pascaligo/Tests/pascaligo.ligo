type storage is int

type parameter is
  Increment of int
| Decrement of int
| Reset

// Two entrypoints

function add (const store : storage; const delta : int) is 
  store + delta

function sub (const store : storage; const delta : int) is 
  store - delta

(* Main access point that dispatches to the entrypoints according to
   the smart contract parameter. *)

function main (const action : parameter; const store : storage) : list(operation) * storage is
 (nil,    // No operations
  case action of [
    Increment (n) -> add (store, n)
  | Decrement (n) -> sub (store, n)
  | Reset         -> 0
  ])

(* Tests for main access point *)

const test_initial_storage = {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate_uncurried(main, initial_storage, 0tez);
  } with assert (Test.get_storage(taddr) = initial_storage);

const test_increment = {
    const initial_storage = 42;
    const (taddr, _, _) = Test.originate_uncurried(main, initial_storage, 0tez);
    const contr = Test.to_contract(taddr);
    const _ = Test.transfer_to_contract_exn(contr, Increment(1), 1mutez);
  } with assert (Test.get_storage(taddr) = initial_storage + 1);
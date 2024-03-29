---
id: dyn_entry
title: dyn_entry
---

import Syntax from '@theme/Syntax'

<Syntax syntax="cameligo">

The attribute `[@dyn_entry]` on a function indicates that said
function should be made available as a *dynamic entrypoint* of the
smart contract.

Dynamic entry points can be removed or updated without deploying a new
contract, unlike entrypoints attributed with `[@entry]` (those cannot
be modified after the contract is originated). Dynamic entrypoints can
be used for example to implement a DAO (Decentralized Autonomous
Organization) with a built-in update mechanism that allows
participants to vote on upgrades to the contract, in a manner somewhat
akin to the amending process of the Tezos protocol itself.

```cameligo group=dyn_entry
type storage = {
  storage : int;
  dynamic_entrypoints;
}

[@dyn_entry]
let one () (_ : int) : operation list * int = [], 1

[@dyn_entry]
let tick : int ticket -> int * int -> operation list * (int * int) =
  fun _ p -> ([], p)
```

</Syntax>

<Syntax syntax="jsligo">

The decorator `@dyn_entry` on a function indicates that said
function should be made available as a *dynamic entrypoint* of the
smart contract.

Dynamic entry points can be removed or updated without deploying a new
contract, unlike entrypoints decorated with `@entry` (those cannot be
modified after the contract is originated). Dynamic entrypoints can be
used for example to implement a DAO (Decentralized Autonomous
Organization) with a built-in update mechanism that allows
participants to vote on upgrades to the contract, in a manner somewhat
akin to the amending process of the Tezos protocol itself.

```jsligo group=dyn_entry
type storage = {
  storage: int;
  dynamic_entrypoints;
};

@dyn_entry
const one = (_u: unit, _i: int): [list<operation>, int] =>
  [list([]), 1];

@dyn_entry
const tick = (_: ticket<int>, x: [int, int])
  : [list<operation>, [int, int]] =>
  [list([]), x];
```

</Syntax>

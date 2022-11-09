---
id: tezos-now-advance-time
title: Is there a way to advance time in Ligo tests ?
---

import Syntax from '@theme/Syntax';

The `Tezos.now()` function can be used to get the current time, but in the tests,
we may want to setup a situation where the contracts thinks `Tezos.now()` is in the future.

Time advances by baking (protocol checks and enforces the timestamp makes sense)

So, to bake and advance time, you can use :
<SyntaxTitle syntax="pascaligo">
val bake_until_n_cycle_end : nat -> unit
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val bake_until_n_cycle_end : nat -> unit
</SyntaxTitle>

<SyntaxTitle syntax="reasonligo">
let bake_until_n_cycle_end: nat => unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let bake_until_n_cycle_end = (cycles : nat) => unit
</SyntaxTitle>


Depending on the situation, the following can be useful as well :
<SyntaxTitle syntax="pascaligo">
val reset_state_at : timestamp -> nat -> list (tez) -> unit
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val reset_state_at : timestamp -> nat -> tez list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="reasonligo">
let reset_state_at: (timestamp, nat, list(tez)) => unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let reset_state_at = (initial_timestamp : timestamp, no_of_accounts: nat, amount: list<tez>) => unit
</SyntaxTitle>


For more information on these function, see the [Test library](../reference/test.md)
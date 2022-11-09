---
id: tezos-now-advance-time
title: Is there a way to advance time in Ligo tests ?
---

import Syntax from '@theme/Syntax';

The `Tezos.now()` function can be used to get the current time, but in the tests,
we may want to setup a situation where the contracts thinks `Tezos.now()` is in the future.

Time advances by baking (protocol checks and enforces the timestamp makes sense)

So, to bake and advance time, you can use :
<Syntax syntax="pascaligo">

```pascaligo
val bake_until_n_cycle_end : nat -> unit
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
val bake_until_n_cycle_end : nat -> unit
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let bake_until_n_cycle_end: nat => unit
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let bake_until_n_cycle_end = (cycles : nat) => unit
```

</Syntax>


Depending on the situation, the following can be useful as well :
<Syntax syntax="pascaligo">

```pascaligo
val reset_state_at : timestamp -> nat -> list (tez) -> unit
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
val reset_state_at : timestamp -> nat -> tez list -> unit
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let reset_state_at: (timestamp, nat, list(tez)) => unit
```

</Syntax>
<Syntax syntax="jsligo">

```jsligo
let reset_state_at = (initial_timestamp : timestamp, no_of_accounts: nat, amount: list<tez>) => unit
```

</Syntax>


For more information on these function, see the [Test library](../reference/test.md)
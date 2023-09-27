---
id: tezos-now-advance-time
title: Is there a way to advance time in LIGO tests ?
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

The `Tezos.get_now` function can be used to get the current time, but
in the tests, we may want to setup a situation where the contracts
thinks `Tezos.get_now()` is in the future.

Time advances by baking (protocol checks and enforces the timestamp makes sense)

<!--
This comment section contains check for signature update in below referenced functions.
Below example is referencing functions in the Test module, but the referencing is done
by plain copy-pasting of the signature.
What if the Test module functions change or get deleted ? The references would be obsolete.

Ideally, we want an mechanism to include code snippets from other parts of the doc,
but without this, we'll do the following hack below.

Below are some assignment that trigger a warning if a signature is outdated.
If you want to refer to `Test.foo`, you add a check like:
  let _dummy : expected_signature_of_foo = Test.foo
And you can mention `foo : expected_signature_of_foo` in the Markdown.
If the function is updated, the typer will fail, triggering a warning,
and you'll have to change the expected signature everywhere it's mentioned in the file.

```cameligo test-ligo group=log
let _dummy : nat -> unit = Test.bake_until_n_cycle_end
let _dummy : timestamp -> nat -> tez list -> unit = Test.reset_state_at
```


```jsligo test-ligo group=log
let _dummy : (cycles : nat) => unit = Test.bake_until_n_cycle_end
let _dummy_2 : (initial_timestamp : timestamp, no_of_accounts: nat, amount: list<tez>) => unit = Test.reset_state_at
```

-->

So, to bake and advance time, you can use:
<SyntaxTitle syntax="cameligo">
val Test.bake_until_n_cycle_end : nat -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let Test.bake_until_n_cycle_end = (cycles : nat) => unit
</SyntaxTitle>

Depending on the situation, the following can be useful as well:
<SyntaxTitle syntax="cameligo">
val Test.reset_state_at : timestamp -> nat -> tez list -> unit
</SyntaxTitle>

<SyntaxTitle syntax="jsligo">
let Test.reset_state_at = (initial_timestamp : timestamp, no_of_accounts: nat, amount: list&lt;tez&gt;) => unit
</SyntaxTitle>


For more information on these function, see the [Test library](../reference/test.md)

<!-- updated use of entry -->
---
id: inline
title: Inlining
---

import Syntax from '@theme/Syntax';

When compiling a contract in LIGO, declarations will get inlined if they are
only used once and pure. Inlining often results in larger contracts and is
therefore not aggressively done.

A pure declaration is one that doesn't cause side effects like causing a
failure or operation.

In some cases you might want to override the default behaviour of LIGO and
force inlining. The declaration still needs to be pure though.

## Inline attribute

To force inlining you can use the inline attribute.

<Syntax syntax="cameligo">

```cameligo

[@inline]
let fst (p : nat * nat) = p.0

[@entry]
let main (p : nat * nat) (s : nat * nat) : operation list * (nat * nat) =
    ([], (fst (p.0, p.1), fst (s.1, s.0)))
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo
// @inline
const fst = (p: [nat, nat]) => p[0];

const main = (p: [nat, nat], s: [nat, nat]) : [list<operation>, [nat, nat]] =>
    [list([]), [fst([p[0], p[1]]), fst([s[1], s[0]])]];
```

</Syntax>

Now if we measure the difference between inlining and without inlining, using
`ligo info measure-contract name_of_contract.mligo --entry-point <entrypoint>`, we see the
following results:

<table>
    <tr>
        <td>With inlining</td><td>66 bytes</td>
    </tr>
    <tr>
        <td>Without inlining</td><td>170 bytes</td>
    </tr>
</table>

:::info
Note that these results can change due to ongoing work to optimise output of
the LIGO compiler.
:::

<!-- updated use of entry -->
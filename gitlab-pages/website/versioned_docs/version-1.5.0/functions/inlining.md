---
id: inlining
title: Inlining
---

import Syntax from '@theme/Syntax';

When compiling a contract in LIGO, declarations will get inlined if
they are only used once and are pure, that is, devoid of side
effects.

Inlining often results in larger contracts and is therefore not
aggressively done. In some cases, you might want to override the
default behaviour of LIGO and force inlining, as long as the
expression considered for inlining is pure, that is, it does not cause
side effects, like possibly failing or a emitting an operation.


<Syntax syntax="cameligo">

To force inlining you can use the `[@inline]` attribute.

```cameligo group=inlining
[@inline]
let fst (p : nat * nat) = p.0

[@entry]
let main (p : nat * nat) (s : nat * nat) : operation list * (nat * nat) =
  [], (fst (p.0, p.1), fst (s.1, s.0))
```

To measure the difference between inlining and without inlining, let
us assume that the above code is inside the file `inline.mligo` and
run the command: `ligo info measure-contract inline.mligo` with and
without the attribute `[@inline]`. We obtain the following results

</Syntax>

<Syntax syntax="jsligo">

To force inlining you can use the `@inline` decorator.

```jsligo group=inlining
@inline
const fst = (p: [nat, nat]) => p[0];

@entry
const main = (p: [nat, nat], s: [nat, nat]) : [list<operation>, [nat, nat]] =>
    [list([]), [fst([p[0], p[1]]), fst([s[1], s[0]])]];
```


To measure the difference between inlining and without inlining, let
us assume that the above code is inside the file `inline.jsligo` and
run the command: `ligo info measure-contract inline.jsligo` with and
without the decorator `@inline`. We obtain the following results

</Syntax>

<table>
    <tr>
        <td>With inlining</td><td>46 bytes</td>
    </tr>
    <tr>
        <td>Without inlining</td><td>97 bytes</td>
    </tr>
</table>

:::info
Note that these results can change due to ongoing work to optimise output of
the LIGO compiler.
:::

---
id: embedded-michelson
title: Embedded Michelson 
---

import Syntax from '@theme/Syntax';

If you have an existing piece of Michelson code that you want to use as-is, 
LIGO provides the ability to embed Michelson code.

<Syntax syntax="pascaligo">

```pascaligo
  function michelson_add (var n : nat * nat ) : nat is block {
    const f : (nat * nat -> nat) = 
      [%Michelson ({| { UNPAIR; ADD } |} : nat *nat -> nat)];
  } with f (n)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let michelson_add (n : nat * nat) : nat =
  [%Michelson ({| { UNPAIR;ADD } |} : nat * nat -> nat) ] n
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let michelson_add = (n : (nat, nat)) : nat =>
  [%Michelson ({| { UNPAIR;ADD } |} : ((nat, nat) => nat)) ](n);
```

</Syntax>

Note that the type annotation is required, because the embedded Michelson code
is not type checked by LIGO. This assumes that the given type is correct. 

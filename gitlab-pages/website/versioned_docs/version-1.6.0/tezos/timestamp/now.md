---
id: now
title: Now
---

import Syntax from '@theme/Syntax';

You can obtain the starting time of the current block using the
function `Tezos.get_now` from the standard library. This timestamp
does not change during the execution of the contract. Please be aware
that it is up to the baker to set the current timestamp value.

<Syntax syntax="cameligo">

```cameligo group=now
let today : timestamp = Tezos.get_now ()
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=now
const today: timestamp = Tezos.get_now();
```

</Syntax>


> When testing your code, the LIGO CLI option `--now` allows you to
> control what `Tezos.get_now` returns.

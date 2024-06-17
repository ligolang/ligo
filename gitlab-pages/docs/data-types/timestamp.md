---
id: timestamp
title: Timestamps
---

import Syntax from '@theme/Syntax';

LIGO features timestamps by means of the predefined type `timestamp`,
as Michelson does. Bakers making the block (including the transaction
in a block) are responsible for providing the given current timestamp
for the contract.

## Now

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

## Arithmetic

In LIGO, timestamps can be added to integers, allowing you to set time
constraints on your smart contracts. Consider the following scenarios.

### Incrementing

<Syntax syntax="cameligo">

```cameligo group=tomorrow
let today : timestamp = Tezos.get_now ()
let one_day : int = 86_400
let in_24_hrs : timestamp = today + one_day
let some_date : timestamp = ("2000-01-01t10:10:10Z" : timestamp)
let one_day_later : timestamp = some_date + one_day
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=tomorrow
const today: timestamp = Tezos.get_now();
const one_day: int = 86_400;
const in_24_hrs: timestamp = today + one_day;
const some_date: timestamp = "2000-01-01t10:10:10Z" as timestamp;
const one_day_later: timestamp = some_date + one_day;
```

</Syntax>


### Decrementing

<Syntax syntax="cameligo">

```cameligo group=yesterday
let today : timestamp = Tezos.get_now ()
let one_day : int = 86400
let in_24_hrs : timestamp = today - one_day
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=yesterday
const today: timestamp = Tezos.get_now();
const one_day: int = 86400;
const in_24_hrs: timestamp = today - one_day;
```

</Syntax>

### Subtracting

Timestamps can be subtracted, that means, we can use minus (`-`)
between two timestamps:

<Syntax syntax="cameligo">

```cameligo group=subtracting
let today : timestamp = Tezos.get_now ()
let some_date : timestamp = ("2035-01-01t10:10:10Z" : timestamp)
let secs_until_some_date : int = some_date - today
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=subtracting
const today: timestamp = Tezos.get_now();
const some_date: timestamp = "2035-01-01t10:10:10Z" as timestamp;
const secs_until_some_date: int = some_date - today;
```

</Syntax>

Notice that the result of such subtraction is an `int`, which describes the difference in seconds between the two timestamps.

## Comparing

You can compare timestamps using the same comparison operators
applying to numerical value.

<Syntax syntax="cameligo">

```cameligo group=comparing
let today : timestamp = Tezos.get_now ()
let one_day : int = 86400
let in_24_hrs : timestamp = today - one_day
let not_tomorrow : bool = (Tezos.get_now () = in_24_hrs)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=comparing
const today: timestamp = Tezos.get_now();
const one_day: int = 86400;
const in_24_hrs: timestamp = today - one_day;
const not_tomorrow: bool = (Tezos.get_now() == in_24_hrs);
```

</Syntax>

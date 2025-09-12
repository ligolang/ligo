---
id: arithmetics
title: Arithmetics
---

import Syntax from '@theme/Syntax';

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

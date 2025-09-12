---
id: switches
title: Switches
---

import Syntax from '@theme/Syntax';

<Syntax syntax="cameligo">
This feature is not available in CameLIGO."
</Syntax>

<Syntax syntax="jsligo">

TypeScript developers are familiar with the `switch` statement. By
contrast, [pattern matching](../variants/matching.md) is a conditional
expression that destructures a value of almost any type to control the
flow of execution. Switches only work on simple types, like `int`,
`nat`, `bool`, `string` etc., which makes them akin to *enumerated
types*. They are most useful when they avoid writing nested
conditional statements.

```jsligo group=switch
function hello (day: nat) : string {
  let greeting = "Hello";
  switch (day) {
    case 1n: greeting += " Monday!"; break;
    case 2n: greeting += " Tuesday!"; break;
    case 3n: greeting += " Wednesday!"; break;
    case 4n: greeting += " Thursday!"; break;
    case 5n: greeting += " Friday!"; break;
    case 6n: greeting += " Saturday!"; break;
    case 7n: greeting += " Sunday!"; break;
    default: greeting += "!"; break;
  };
  return greeting;
}
```

Each case is introduced by the keyword `case`, except the `default`
case, which must be last and applies when all the previous cases have
failed to match.

</Syntax>

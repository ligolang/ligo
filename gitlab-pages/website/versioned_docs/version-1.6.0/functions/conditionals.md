---
id: conditionals
title: Conditional expressions
---

import Syntax from '@theme/Syntax';

Conditional logic enables forking the control flow depending on the
state, that is, the values available at a given point in the code. Put
in a less technical manner, conditionals enable decision making.

A conditional expression is made of three parts:
<ol>
  <li> a condition, that is, a boolean expression;</li>
  <li> an expression evaluated if, and only if, the condition is true;</li>
  <li> an expression evaluated if, and only if, the condition is false.</li>
</ol>

<Syntax syntax="cameligo">

The syntax uses the keywords `if`, `then` and `else` to separate the
three parts, like so:

```cameligo group=conditionals
let a = 0
let b = 1
let min = if a < b then a else b // min = 0
```

</Syntax>

<Syntax syntax="jsligo">

The syntax uses a ternary operator with the symbols `?` and `:` to
separate the three parts:

```jsligo group=conditionals
const a = 0;
const b = 1;
const min = (a < b) ? a : b; // min == 0
```

Note: Parentheses are often necessary before `?`, but not always: you
can either rely on the compiler error message or always use
parentheses.

</Syntax>

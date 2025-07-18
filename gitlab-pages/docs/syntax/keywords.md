---
id: keywords
title: Keywords
---

import Syntax from '@theme/Syntax';

_Keywords_ are reserved words that cannot be used as names in declarations such as variables and record fields.

<Syntax syntax="cameligo">

## Escaping keywords

If you need to use a keyword as a variable name, you can prefix it with `@`, as in this example:

```cameligo group=keywords
let @from = ("tz1fakefakefakefakefakefakefakcphLA5" : address)
```

</Syntax>

<Syntax syntax="jsligo">

Unlike in previous versions of JsLIGO, you cannot start variable names with an `@` symbol and therefore you cannot escape keywords by putting an `@` symbol in front of their names.
You can modify the keywords in other ways, such as adding an underscore to the end.

</Syntax>

## List of keywords

<Syntax syntax="jsligo">

JsLIGO's keywords are the following:

- `as`
- `break`
- `case`
- `const`
- `continue`
- `contract_of`
- `default`
- `do`
- `else`
- `export`
- `false`
- `for`
- `from`
- `function`
- `if`
- `implements`
- `import`
- `interface`
- `let`
- `match`
- `namespace`
- `of`
- `parameter_of`
- `return`
- `switch`
- `true`
- `type`
- `when`
- `while`

JslIGO may allow you to create and use variables with these names in some cases, but for some uses the compiler fails to compile contracts that use variables with these names.
Therefore, do not use variables with these names even if the contract appears to work.

</Syntax>

<Syntax syntax="cameligo">
CameLIGO's keywords are the following:
<ul>
  <li> <code>begin</code> </li>
  <li> <code>do</code> </li>
  <li> <code>done</code> </li>
  <li> <code>downto</code> </li>
  <li> <code>else</code> </li>
  <li> <code>end</code> </li>
  <li> <code>false</code> </li>
  <li> <code>for</code> </li>
  <li> <code>fun</code> </li>
  <li> <code>if</code> </li>
  <li> <code>in</code> </li>
  <li> <code>include</code> </li>
  <li> <code>land</code> </li>
  <li> <code>let</code> </li>
  <li> <code>lor</code> </li>
  <li> <code>lsl</code> </li>
  <li> <code>lsr</code> </li>
  <li> <code>lxor</code> </li>
  <li> <code>match</code> </li>
  <li> <code>mod</code> </li>
  <li> <code>module</code> </li>
  <li> <code>mut</code> </li>
  <li> <code>not</code> </li>
  <li> <code>of</code> </li>
  <li> <code>or</code> </li>
  <li> <code>rec</code> </li>
  <li> <code>sig</code> </li>
  <li> <code>struct</code> </li>
  <li> <code>then</code> </li>
  <li> <code>true</code> </li>
  <li> <code>type</code> </li>
  <li> <code>val</code> </li>
  <li> <code>while</code> </li>
  <li> <code>with</code> </li>
</ul>
</Syntax>

---
id: keywords
title: Keywords
---

import Syntax from '@theme/Syntax';

_Keywords_ are reserved words that cannot be used as names in declarations.
In some cases you can escape keywords to use them as variables or record fields.

## Escaping keywords

Keywords cannot be used as variables or record fields. If you need to
use a keyword as a variable, you can prefix it with `@`, like so:

<Syntax syntax="cameligo">

```cameligo group=keywords
let @from = ("tz1fakefakefakefakefakefakefakcphLA5" : address)
```

</Syntax>

<Syntax syntax="jsligo">

```jsligo group=keywords
const @from = ("tz1fakefakefakefakefakefakefakcphLA5" as address)
```

However, you cannot escape decorators in this way because the convention of escaping a keyword with the `@` symbol conflicts with that of *decorators*, as found in JavaScript.
For example, `@entry` is invalid as a variable.

</Syntax>

## List of keywords

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

<Syntax syntax="jsligo">
JsLIGO's keywords are the following:
<ul>
  <li> <code>as</code> </li>
  <li> <code>break</code> </li>
  <li> <code>case</code> </li>
  <li> <code>const</code> </li>
  <li> <code>continue</code> </li>
  <li> <code>contract_of</code> </li>
  <li> <code>default</code> </li>
  <li> <code>do</code> </li>
  <li> <code>else</code> </li>
  <li> <code>export</code> </li>
  <li> <code>false</code> </li>
  <li> <code>for</code> </li>
  <li> <code>from</code> </li>
  <li> <code>function</code> </li>
  <li> <code>if</code> </li>
  <li> <code>implements</code> </li>
  <li> <code>import</code> </li>
  <li> <code>interface</code> </li>
  <li> <code>let</code> </li>
  <li> <code>match</code> </li>
  <li> <code>namespace</code> </li>
  <li> <code>of</code> </li>
  <li> <code>parameter_of</code> </li>
  <li> <code>return</code> </li>
  <li> <code>switch</code> </li>
  <li> <code>true</code> </li>
  <li> <code>type</code> </li>
  <li> <code>when</code> </li>
  <li> <code>while</code> </li>
  <li> <code>@entry</code> </li>
  <li> <code>@deprecated</code> </li>
  <li> <code>@dyn_entry</code> </li>
  <li> <code>@inline</code> </li>
  <li> <code>@view</code> </li>
  <li> <code>@no_mutation</code> </li>
  <li> <code>@private</code> </li>
  <li> <code>@public</code> </li>
  <li> <code>@hidden</code> </li>
  <li> <code>@thunk</code> </li>
  <li> <code>@annot</code> </li>
  <li> <code>@layout</code> </li>
</ul>
</Syntax>

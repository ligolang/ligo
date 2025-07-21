---
id: test.assert.error-reference
title: error
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


<SyntaxTitle syntax="cameligo">
val assert : bool -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
assert: (condition: bool, error: string) =&gt; unit
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `assert cond error` terminates the execution with the string
`error` (that is, an error message) if, and only if, the boolean
condition `cond` is false. The failure is handled by LIGO's testing
framework and not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert(cond, error)` terminates the execution with the
string `error` (that is, an error message) if, and only if, the
boolean condition `cond` is false. The failure is handled by LIGO's
testing framework and not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val some : &#39;a.&#39;a option -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
some: &lt;a&gt;(_: option&lt;a&gt;, error: string) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `some opt err` terminates the execution with the string `err`
(that is, an error message) if, and only if, `opt` is `None`. The
failure is handled by LIGO's testing framework and not by Michelson's
interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `some(opt, err)` terminates the execution with the string
`err` (that is, an error message) if, and only if, `opt` is
`["None" as "None"]`. The failure is handled by LIGO's testing
framework and not by Michelson's interpreter.

</Syntax>


<SyntaxTitle syntax="cameligo">
val none : &#39;a.&#39;a option -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
none: &lt;a&gt;(_: option&lt;a&gt;, error: string) =&gt; unit
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `none opt err` terminates the execution with the string `err`
(that is, an error message) if, and only if, `opt` is an optional
value different from `None`. The failure is handled by LIGO's testing
framework and not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `none(opt, err)` terminates the execution with the string
`err` (that is, an error message) if, and only if, `opt` is an
optional value different from `["None" as "None"]`.  The failure is
handled by LIGO's testing framework and not by Michelson's
interpreter.

</Syntax>

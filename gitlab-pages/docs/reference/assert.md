---
id: assert-reference
title: assert
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



[module error](assert.error.md)


<SyntaxTitle syntax="cameligo">
val assert : bool -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert: (&#95;: bool) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `assert cond` terminates the execution with the string
    `"failed assertion"` if, and only if, the boolean condition `cond`
    is false.

</Syntax>

<Syntax syntax="jsligo">

The call `assert(cond)` terminates the execution with the string
    `"failed assertion"` if, and only if, the boolean condition `cond`
    is false.

</Syntax>


<SyntaxTitle syntax="cameligo">
val some : &#39;a.&#39;a option -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let some: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `some opt` terminates the execution with the
    string `"failed assert some"` if, and only if, `opt` is `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `some(opt)` terminates the execution with the
    string `"failed assert some"` if, and only if, `opt` is `None()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val none : &#39;a.&#39;a option -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let none: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `none opt` terminates the execution with the string
    `"failed assert none"` if, and only if, `opt` is not `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `none(opt)` terminates the execution with the string
    `"failed assert none"` if, and only if, `opt` is not `None()`.

</Syntax>

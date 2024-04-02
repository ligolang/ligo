---
id: test.next.assert-reference
title: Assert
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



[module Error](Test.Next.Assert.Error.md)


<SyntaxTitle syntax="cameligo">
val failwith : &#39;a &#39;b.&#39;a -&gt; &#39;b
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let failwith: &lt;a, b&gt;(&#95;: a) =&gt; b
</SyntaxTitle>
Cause the testing framework to fail.


<SyntaxTitle syntax="cameligo">
val assert : bool -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert: (&#95;: bool) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `assert cond` terminates the execution with the string
        `"failed assertion"` if, and only if, the boolean condition `cond`
        is false. The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `assert(cond)` terminates the execution with the string
        `"failed assertion"` if, and only if, the boolean condition `cond`
        is false. The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter.

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
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `some(opt)` terminates the execution with the
        string `"failed assert some"` if, and only if, `opt` is `None()`.
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter.

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
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter.

</Syntax>

<Syntax syntax="jsligo">

The call `none(opt)` terminates the execution with the string
        `"failed assert none"` if, and only if, `opt` is not `None()`.
        The failure is handled by LIGO's testing framework and
        not by Michelson's interpreter.

</Syntax>

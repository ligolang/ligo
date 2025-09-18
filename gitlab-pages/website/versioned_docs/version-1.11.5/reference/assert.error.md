---
id: assert.error-reference
title: error
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val assert : bool -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let assert: (&#95;: bool) =&gt; (&#95;: string) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `assert cond error` terminates the execution
      with the string `error` (that is, an error message) if, and only if,
      the boolean condition `cond` is false.

</Syntax>

<Syntax syntax="jsligo">

The call `assert(cond, error)` terminates the execution
      with the string `error` (that is, an error message) if, and only if,
      the boolean condition `cond` is false.

</Syntax>


<SyntaxTitle syntax="cameligo">
val some : &#39;a.&#39;a option -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let some: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; (&#95;: string) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `some opt err` terminates the execution
      with the string `err` (that is, an error message) if, and only if,
      `opt` is `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `some(opt, err)` terminates the execution
      with the string `err` (that is, an error message) if, and only if,
      `opt` is `None()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val none : &#39;a.&#39;a option -&gt; string -&gt; unit
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let none: &lt;a&gt;(&#95;: option&lt;a&gt;) =&gt; (&#95;: string) =&gt; unit
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `none opt err` terminates the execution
      with the string `err` (that is, an error message) if, and only if,
      `opt` is an optional value different from `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `none(opt, err)` terminates the execution
      with the string `err` (that is, an error message) if, and only if,
      `opt` is an optional value different from `None()`.

</Syntax>

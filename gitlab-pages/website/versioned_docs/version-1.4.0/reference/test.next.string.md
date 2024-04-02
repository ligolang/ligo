---
id: test.next.string-reference
title: String
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val chr : nat -&gt; string option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let chr: (&#95;: nat) =&gt; option&lt;string&gt;
</SyntaxTitle>
String consisting of the character represented by a `nat` in the
        interval `[0, 255]`.


<SyntaxTitle syntax="cameligo">
val nl : string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let nl: string
</SyntaxTitle>
String consisting of only a newline.


<SyntaxTitle syntax="cameligo">
val show : &#39;a.&#39;a -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let show: &lt;a&gt;(&#95;: a) =&gt; string
</SyntaxTitle>
Converts a value to a string (same conversion as used by
        `Test.log`).


<SyntaxTitle syntax="cameligo">
val json : &#39;a.&#39;a -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let json: &lt;a&gt;(&#95;: a) =&gt; string
</SyntaxTitle>
Converts a value to its JSON representation (as a string).


<SyntaxTitle syntax="cameligo">
val debugger&#95;json : &#39;a.&#39;a -&gt; string
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let debugger&#95;json: &lt;a&gt;(&#95;: a) =&gt; string
</SyntaxTitle>

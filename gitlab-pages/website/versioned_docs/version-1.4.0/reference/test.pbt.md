---
id: test.pbt-reference
title: PBT
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val gen : &#39;a.&#39;a pbt&#95;gen
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let gen: &lt;a&gt;pbt&#95;gen&lt;a&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val gen&#95;small : &#39;a.&#39;a pbt&#95;gen
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let gen&#95;small: &lt;a&gt;pbt&#95;gen&lt;a&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val make&#95;test : &#39;a.&#39;a pbt&#95;gen -&gt; (&#39;a -&gt; bool) -&gt; &#39;a pbt&#95;test
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let make&#95;test: &lt;a&gt;(&#95;: pbt&#95;gen&lt;a&gt;) =&gt; (&#95;: (&#95;: a) =&gt; bool) =&gt; pbt&#95;test&lt;a&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val run : &#39;a.&#39;a pbt&#95;test -&gt; nat -&gt; &#39;a pbt&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let run: &lt;a&gt;(&#95;: pbt&#95;test&lt;a&gt;) =&gt; (&#95;: nat) =&gt; pbt&#95;result&lt;a&gt;
</SyntaxTitle>

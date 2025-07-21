---
id: test.pbt-reference
title: pbt
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val gen : &#39;a.&#39;a pbt_gen
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
gen: &lt;a&gt;pbt_gen&lt;a&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val gen_small : &#39;a.&#39;a pbt_gen
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
gen_small: &lt;a&gt;pbt_gen&lt;a&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val make_test : &#39;a.&#39;a pbt_gen -&gt; (&#39;a -&gt; bool) -&gt; &#39;a pbt_test
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
make_test: &lt;a&gt;(_: pbt_gen&lt;a&gt;, predicate: (_: a) =&gt; bool) =&gt; pbt_test&lt;a&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val run : &#39;a.&#39;a pbt_test -&gt; nat -&gt; &#39;a pbt_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
run: &lt;a&gt;(_: pbt_test&lt;a&gt;, _: nat) =&gt; pbt_result&lt;a&gt;
</SyntaxTitle>

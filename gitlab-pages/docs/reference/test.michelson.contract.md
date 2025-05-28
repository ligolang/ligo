---
id: test.michelson.contract-reference
title: contract
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val compile : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
compile : &lt;p, s&gt;(&#95;: (&#95;: [p, s]) =&gt; [list&lt;operation&gt;, s]) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>

Compiles a contract from an entrypoint function.

<SyntaxTitle syntax="cameligo">
val compile&#95;with&#95;views : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; &#39;s views -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
compile&#95;with&#95;views:
  &lt;p, s&gt;(&#95;: (&#95;: [p, s]) =&gt; [list&lt;operation&gt;, s]) =&gt; (&#95;: views&lt;s&gt;) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>

Compiles a contract with given views.

<SyntaxTitle syntax="cameligo">
val size : &#39;p &#39;s.(&#39;p, &#39;s) michelson&#95;contract -&gt; int
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
size: &lt;p, s&gt;(&#95;: michelson&#95;contract&lt;p, s&gt;) =&gt; int
</SyntaxTitle>

Measures the size of a contract.


<SyntaxTitle syntax="cameligo">
val from&#95;file : &#39;p &#39;s.string -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
from&#95;file: &lt;p, s&gt;(&#95;: string) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>

Reads a contract from a `.tz` file.

<SyntaxTitle syntax="cameligo">
val compile&#95;from&#95;file : &#39;p &#39;s.string -&gt; (&#39;p, &#39;s) michelson&#95;contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
compile&#95;from&#95;file: &lt;p, s&gt;(&#95;: string) =&gt; michelson&#95;contract&lt;p, s&gt;
</SyntaxTitle>

Compiles a contract with a path to the contract file, an entrypoint,
and a list of views.

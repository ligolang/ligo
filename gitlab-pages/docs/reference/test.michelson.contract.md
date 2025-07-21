---
id: test.michelson.contract-reference
title: contract
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val compile : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; (&#39;p, &#39;s) michelson_contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
compile : &lt;p, s&gt;(_: (_: [p, s]) =&gt; [list&lt;operation&gt;, s]) =&gt; michelson_contract&lt;p, s&gt;
</SyntaxTitle>

Compiles a contract from an entrypoint function.

<SyntaxTitle syntax="cameligo">
val compile_with_views : &#39;p &#39;s.((&#39;p * &#39;s) -&gt; (operation list * &#39;s)) -&gt; &#39;s views -&gt; (&#39;p, &#39;s) michelson_contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
compile_with_views:
  &lt;p, s&gt;(f: (_: [p, s]) =&gt; [list&lt;operation&gt;, s], v: views&lt;s&gt;) =&gt; michelson_contract&lt;p, s&gt;
</SyntaxTitle>

Compiles a contract with given views.

<SyntaxTitle syntax="cameligo">
val size : &#39;p &#39;s.(&#39;p, &#39;s) michelson_contract -&gt; int
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
size: &lt;p, s&gt;(_: michelson_contract&lt;p, s&gt;) =&gt; int
</SyntaxTitle>

Measures the size of a contract.


<SyntaxTitle syntax="cameligo">
val from_file : &#39;p &#39;s.string -&gt; (&#39;p, &#39;s) michelson_contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
from_file: &lt;p, s&gt;(michelson_file: string) =&gt; michelson_contract&lt;p, s&gt;
</SyntaxTitle>

Reads a contract from a `.tz` file.

<SyntaxTitle syntax="cameligo">
val compile_from_file : &#39;p &#39;s.string -&gt; (&#39;p, &#39;s) michelson_contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
compile_from_file: &lt;p, s&gt;(contract_file: string) =&gt; michelson_contract&lt;p, s&gt;
</SyntaxTitle>

Compiles a contract with a path to the contract file, an entrypoint,
and a list of views.

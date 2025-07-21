---
id: test.originate-reference
title: originate
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


<SyntaxTitle syntax="cameligo">
val michelson : &#39;p &#39;s.(&#39;p, &#39;s) michelson_contract -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) typed_address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
michelson: &lt;p, s&gt;(_: michelson_contract&lt;p, s&gt;,
storage: s, balance: tez) =&gt; typed_address&lt;p, s&gt;
</SyntaxTitle>

Originate a contract with initial storage and initial balance.

<SyntaxTitle syntax="cameligo">
val contract : &#39;p &#39;s.(&#39;p, &#39;s) module_contract -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) origination_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
contract: &lt;p, s&gt;(_: module_contract&lt;p, s&gt;, storage: s, balance: tez) =&gt; origination_result&lt;p, s&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

Originate a contract with an entrypoint function in curried form,
initial storage and initial balance.

</Syntax>

<Syntax syntax="jsligo">

Originate a contract with an entrypoint, initial storage and
initial balance.

</Syntax>


<SyntaxTitle syntax="cameligo">
val from_file : &#39;p &#39;s.string -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) origination_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
from_file: &lt;p, s&gt;(path: string, storage: s, balance: tez) =&gt; origination_result&lt;p, s&gt;
</SyntaxTitle>

Originate a contract with a path to the contract file, an entrypoint,
and a list of views, together with an initial storage and an initial
balance.

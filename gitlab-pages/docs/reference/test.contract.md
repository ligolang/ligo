---
id: test.contract-reference
title: contract
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


<SyntaxTitle syntax="cameligo">
val transfer : &#39;p.&#39;p contract -&gt; &#39;p -&gt; tez -&gt; test&#95;exec&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
transfer: &lt;p&gt;(&#95;: contract&lt;p&gt;, param: p, amount: tez) =&gt; test&#95;exec&#95;result
</SyntaxTitle>

Bake a transaction by sending an amount of tez with a parameter from
the current source to a contract. Returns the amount of gas consumed
by the execution of the contract.

<SyntaxTitle syntax="cameligo">
val transfer&#95;exn : &#39;p.&#39;p contract -&gt; &#39;p -&gt; tez -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
transfer&#95;exn: &lt;p&gt;(&#95;: contract&lt;p&gt;, param: p, amount: tez) =&gt; nat
</SyntaxTitle>

Bakes a transaction by sending an amount of tez with a parameter from
the current source to a contract. Returns the amount of gas consumed
by the execution of the contract. Similar to `transfer`, but fails
when anything goes wrong.

<SyntaxTitle syntax="cameligo">
val to&#95;typed&#95;address : &#39;p &#39;s.&#39;p contract -&gt; (&#39;p, &#39;s) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
to&#95;typed&#95;address: &lt;p, s&gt;(&#95;: contract&lt;p&gt;) =&gt; typed&#95;address&lt;p, s&gt;
</SyntaxTitle>

---
id: test.next.account.contract-reference
title: Contract
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val bootstrap : nat -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let bootstrap: (&#95;: nat) =&gt; address
</SyntaxTitle>
Returns the address corresponding to the nth bootstrapped
          contract.


<SyntaxTitle syntax="cameligo">
val bootstrap&#95;typed&#95;address : &#39;a &#39;b.nat -&gt; (&#39;a, &#39;b) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let bootstrap&#95;typed&#95;address: &lt;a, b&gt;(&#95;: nat) =&gt; typed&#95;address&lt;a, b&gt;
</SyntaxTitle>
Returns the typed address corresponding to the nth bootstrapped
          contract currently loaded. The types are inferred from those
          contracts loaded with `Test.State.Reset.add_func_contract`
	  (before reset).

---
id: test.next.address-reference
title: Address
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val get&#95;balance : address -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;balance: (&#95;: address) =&gt; tez
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val to&#95;typed&#95;address : &#39;a &#39;b.address -&gt; (&#39;a, &#39;b) typed&#95;address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let to&#95;typed&#95;address: &lt;a, b&gt;(&#95;: address) =&gt; typed&#95;address&lt;a, b&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val get&#95;storage : &#39;b.address -&gt; &#39;b
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let get&#95;storage: &lt;b&gt;(&#95;: address) =&gt; b
</SyntaxTitle>

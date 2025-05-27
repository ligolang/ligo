---
id: test.next-reference
title: next
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



[module mutation](test.next.mutation.md)


module PBT = PBT

[module state](test.next.state.md)


[module account](test.next.account.md)


[module compare](test.next.compare.md)


[module michelson](test.next.michelson.md)


[module io](test.next.io.md)


[module assert](test.next.assert.md)


[module string](test.next.string.md)


[module ticket](test.next.ticket.md)


[module originate](test.next.originate.md)


[module contract](test.next.contract.md)


[module typed_address](test.next.typed_address.md)


[module address](test.next.address.md)


[module timelock](test.next.timelock.md)


[module crypto](test.next.crypto.md)


[module dynamic_entrypoints](test.next.dynamic_entrypoints.md)


<SyntaxTitle syntax="cameligo">
val originate : &#39;p &#39;s.(&#39;p, &#39;s) module&#95;contract -&gt; &#39;s -&gt; tez -&gt; (&#39;p, &#39;s) origination&#95;result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let originate: &lt;p, s&gt;(&#95;: module&#95;contract&lt;p, s&gt;) =&gt; (&#95;: s) =&gt; (&#95;: tez) =&gt; origination&#95;result&lt;p, s&gt;
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val failwith : &#39;a &#39;b.&#39;a -&gt; &#39;b
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let failwith: &lt;a, b&gt;(&#95;: a) =&gt; b
</SyntaxTitle>

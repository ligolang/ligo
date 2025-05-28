---
id: test-reference
title: test
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';


The testing framework


[module michelson](test.michelson.md)

[module originate](test.originate.md)

[module mutation](test.mutation.md)

[module pbt](test.pbt.md)

[module String](test.string.md)

[module IO](test.io.md)

[module Typed_address](test.typed_address.md)

[module State](test.state.md)

[module Account](test.account.md)

[module Compare](test.compare.md)

[module Assert](test.assert.md)

[module Contract](test.contract.md)

[module Address](test.address.md)

[module Ticket](test.ticket.md)

[module Timelock](test.timelock.md)

[module Crypto](test.crypto.md)

[module Dynamic_entrypoints](test.dynamic_entrypoints.md)

<SyntaxTitle syntax="cameligo">
val random : &#39;a.unit -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
random: &lt;a&gt;(&#95;: unit) =&gt; a
</SyntaxTitle>

This function creates a random value for a chosen type.

<SyntaxTitle syntax="cameligo">
val get_time : &#39;a.unit -&gt; &#39;a
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_time: &lt;a&gt;(&#95;: unit) =&gt; a
</SyntaxTitle>

Alias of `Tezos.get_now`.

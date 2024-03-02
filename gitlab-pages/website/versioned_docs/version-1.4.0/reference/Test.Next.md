---
id: test.next-reference
title: Next
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



[module Mutation](Test.Next.Mutation.md)


module PBT = [Test.PBT](Test.PBT.md)

[module State](Test.Next.State.md)


[module Account](Test.Next.Account.md)


[module Compare](Test.Next.Compare.md)


[module Michelson](Test.Next.Michelson.md)


[module IO](Test.Next.IO.md)


[module Assert](Test.Next.Assert.md)


[module String](Test.Next.String.md)


[module Ticket](Test.Next.Ticket.md)


[module Originate](Test.Next.Originate.md)


[module Contract](Test.Next.Contract.md)


[module Typed_address](Test.Next.Typed_address.md)


[module Address](Test.Next.Address.md)


[module Timelock](Test.Next.Timelock.md)


[module Crypto](Test.Next.Crypto.md)


[module Dynamic_entrypoints](Test.Next.Dynamic_entrypoints.md)


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

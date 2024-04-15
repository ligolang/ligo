---
id: tezos.next.ticket-reference
title: ticket
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val create : &#39;a.&#39;a -&gt; nat -&gt; &#39;a ticket option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let create: &lt;a&gt;(&#95;: a) =&gt; (&#95;: nat) =&gt; option&lt;ticket&lt;a&gt;&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `create v a` creates a ticket with value `v` and
        amount `a`. If the creation is a success, the value `Some t` is
        returned, where `t` is the ticket; otherwise, `None` is the
        result. Note: Tickets cannot be duplicated.

</Syntax>

<Syntax syntax="jsligo">

The call `create(v, a)` creates a ticket with value `v` and
        amount `a`. If the creation is a success, the value `Some(t)` is
        returned, where `t` is the ticket; otherwise, `None()` is the
        result. Note: Tickets cannot be duplicated.

</Syntax>


<SyntaxTitle syntax="cameligo">
val split : &#39;a.&#39;a ticket -&gt; (nat * nat) -&gt; (&#39;a ticket * &#39;a ticket) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let split: &lt;a&gt;(&#95;: ticket&lt;a&gt;) =&gt; (&#95;: [nat, nat]) =&gt; option&lt;[ticket&lt;a&gt;, ticket&lt;a&gt;]&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `split t (a1, a2)` results in a pair of tickets
        `t1` and `t2` such that the former owns the amount `a1` and the
        later `a2`. More precisely, the value of the call is
        `Some (t1, t2)` because signifying to the callee the failure of
        the splitting is achieved by returning the value `None`.

</Syntax>

<Syntax syntax="jsligo">

The call `split(t, [a1, a2])` results in a pair of tickets
        `t1` and `t2` such that the former owns the amount `a1` and the
        later `a2`. More precisely, the value of the call is
        `Some([t1, t2])` because signifying to the callee the failure of
        the splitting is achieved by returning the value `None()`.

</Syntax>


<SyntaxTitle syntax="cameligo">
val join : &#39;a.(&#39;a ticket * &#39;a ticket) -&gt; &#39;a ticket option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let join: &lt;a&gt;(&#95;: [ticket&lt;a&gt;, ticket&lt;a&gt;]) =&gt; option&lt;ticket&lt;a&gt;&gt;
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `join (t1, t2)` joins the tickets `t1` and
        `t2`, which must have the same type of value.

</Syntax>

<Syntax syntax="jsligo">

The call `join(t1, t2)` joins the tickets `t1` and
        `t2`, which must have the same type of value.

</Syntax>


<SyntaxTitle syntax="cameligo">
val read : &#39;a.&#39;a ticket -&gt; (address * &#39;a * nat * &#39;a ticket)
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
let read: &lt;a&gt;(&#95;: ticket&lt;a&gt;) =&gt; [[address, [a, nat]], ticket&lt;a&gt;]
</SyntaxTitle>
<Syntax syntax="cameligo">

The call `read t` returns `t` itself and the contents of
        `t` which is a pair `(address, (value, amount))`, where `address` is
        the address of the smart contract that created it.

</Syntax>

<Syntax syntax="jsligo">

The call `read(t)` returns `t` itself and the contents of
        `t` which is a pair `[address, [value, amount]]`, where `address` is
        the address of the smart contract that created it.

</Syntax>

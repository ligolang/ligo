---
id: test.typed-address-reference
title: typed_address
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

<SyntaxTitle syntax="cameligo">
val to_contract : &#39;p &#39;s.(&#39;p, &#39;s) typed_address -&gt; &#39;p contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
to_contract: &lt;p, s&gt;(_: typed_address&lt;p, s&gt;) =&gt; contract&lt;p&gt;
</SyntaxTitle>

Gets the contract corresponding to the default entrypoint of a typed
address: the contract parameter in the result will be the type of the
default entrypoint (generally `'param`, but this might differ if
`'param` includes a "default" entrypoint).


<SyntaxTitle syntax="cameligo">
val transfer : &#39;p &#39;s.(&#39;p, &#39;s) typed_address -&gt; &#39;p -&gt; tez -&gt; test_exec_result
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
transfer: &lt;p, s&gt;(_: typed_address&lt;p, s&gt;, param: p, amount: tez) =&gt; test_exec_result
</SyntaxTitle>

Bakes a transaction by sending an amount of tez with a parameter from
the current source to another account. Returns the amount of gas
consumed by the execution of the contract.


<SyntaxTitle syntax="cameligo">
val transfer_exn : &#39;p &#39;s.(&#39;p, &#39;s) typed_address -&gt; &#39;p -&gt; tez -&gt; nat
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
transfer_exn: &lt;p, s&gt;(_: typed_address&lt;p, s&gt;, param: p, amount: tez) =&gt; nat
</SyntaxTitle>

Bakes a transaction by sending an amount of tez with a parameter from
the current source to another account. Returns the amount of gas
consumed by the execution of the contract. Similar as `Test.transfer`,
but fails when anything goes wrong.


<SyntaxTitle syntax="cameligo">
val get_storage : &#39;p &#39;s.(&#39;p, &#39;s) typed_address -&gt; &#39;s
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_storage: &lt;p, s&gt;(_: typed_address&lt;p, s&gt;) =&gt; s
</SyntaxTitle>

Gets the storage of a typed account.


<SyntaxTitle syntax="cameligo">
val to_address : &#39;p &#39;s.(&#39;p, &#39;s) typed_address -&gt; address
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
to_address: &lt;p, s&gt;(_: typed_address&lt;p, s&gt;) =&gt; address
</SyntaxTitle>

Casting a typed address to a regular address.

<SyntaxTitle syntax="cameligo">
val get_balance : &#39;p &#39;s.(&#39;p, &#39;s) typed_address -&gt; tez
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_balance: &lt;p, s&gt;(_: typed_address&lt;p, s&gt;) =&gt; tez
</SyntaxTitle>

Gets the balance of an account in tez.


<SyntaxTitle syntax="cameligo">
val get_entrypoint : &#39;p &#39;s &#39;q.string -&gt; (&#39;p, &#39;s) typed_address -&gt; &#39;q contract
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
get_entrypoint: &lt;p, s, q&gt;(entrypoint: string, _: typed_address&lt;p, s&gt;) =&gt; contract&lt;q&gt;
</SyntaxTitle>

Gets the contract corresponding to an entrypoint of a typed address:
the contract parameter in the result will be the type of the
entrypoint, it needs to be annotated, entrypoint string should omit
the prefix "%", but if passed a string starting with "%", it will be
removed (and a warning emitted).

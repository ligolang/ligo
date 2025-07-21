---
id: tezos.sapling-reference
title: sapling
hide_table_of_contents: true
---
import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';



<SyntaxTitle syntax="cameligo">
val empty&#95;state : &#39;sap&#95;t.&#39;sap&#95;t sapling&#95;state
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
empty&#95;state: &lt;sap&#95;t&gt;sapling&#95;state&lt;sap&#95;t&gt;
</SyntaxTitle>

The evaluation of the constant `empty_state` is an empty
sapling state, that is, no one can spend tokens from it.


<SyntaxTitle syntax="cameligo">
val verify_update :
  &#39;sap_a.&#39;sap_a sapling_transaction -&gt; &#39;sap_a sapling_state -&gt; (bytes * int * &#39;sap_a sapling_state) option
</SyntaxTitle>
<SyntaxTitle syntax="jsligo">
verify_update:
  &lt;sap_a&gt;(trans: sapling_transaction&lt;sap_a&gt;, state: sapling_state&lt;sap_a&gt;) =&gt; option&lt;[bytes, [int, sapling_state&lt;sap_a&gt;]]&gt;
</SyntaxTitle>

<Syntax syntax="cameligo">

The call `verify_update trans state`, where the
transaction `trans` can be applied to the state `state`, returns
`Some (data, (delta, new_state))`, where `data` is the bound data
(as bytes), `delta` is the difference between the outputs and the
inputs of the transaction, and `new_state` is the updated state.

</Syntax>

<Syntax syntax="jsligo">

The call `verify_update(trans, state)`, where the transaction
`trans` can be applied to the state `state`, returns `["Some"
as "Some", [data, [delta, new_state]]]`, where `data` is the
bound data (as bytes), `delta` is the difference between the
outputs and the inputs of the transaction, and `new_state` is
the updated state.

</Syntax>
